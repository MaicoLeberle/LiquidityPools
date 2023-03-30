{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TupleSections      #-}

module Main where

import           Control.Monad
import           Control.Monad.Extra
import           Data.List           ( zip4
                                     , zip5
                                     )
import           Data.Maybe          ( isJust
                                     , fromJust
                                     )
import qualified Data.Set as Set
import           System.Random       (randomRIO)

import Business (newUserID)


main :: IO ()
main = do users <- generateUsers numberOfUsers passLength
          accounts <- generateAccounts users
          pools <- generatePools numberOfPools
          let tokens = generateTokens pools
          ownerships <- generateOwnerships numberOfOwnerships tokens accounts
          writeFile fp $ concat [ insertUsers users
                                , insertAccounts accounts
                                , insertPools pools
                                , insertTokens tokens
                                , insertOwnerships ownerships
                                ]
  where
    fp :: FilePath
    fp = "./db/liquidityPools-insert.sql"

    numberOfUsers :: Int
    numberOfUsers = 100

    passLength :: Int
    passLength = 30

    numberOfPools :: Int
    numberOfPools = 6

    numberOfOwnerships :: Int
    numberOfOwnerships = 20

generateUsers :: Int -> Int -> IO [String]
generateUsers n = replicateM n . newUserID

insertUsers :: [String] -> String
insertUsers ss =
    concat [ "INSERT INTO user_id VALUES ('" ++ uID ++ "');\n" | uID <- ss ]

generateAccounts :: [String] -> IO [Account]
generateAccounts ss = do
    curr <- map getCurrName <$> replicateM (length ss) (randomRIO (0, 3))
    amounts <- replicateM (length ss) (randomRIO @Integer (1, maxAmount))
    return $ zip3 curr amounts ss

insertAccounts :: [Account] -> String
insertAccounts = concat . map insertAccount
  where
    insertAccount :: (String, Integer, String) -> String
    insertAccount (c, a, u) =
        concat [ "INSERT INTO account VALUES ('"
               , c
               , "', "
               , show a
               , ", '"
               , u
               , "');\n"
               ]

-- | Name of asset A must be lexicographically smaller than name of asset B.
generatePools :: Int -> IO [Pool]
generatePools n = do (aNames, bNames) <- mkAssetNames
                     aAmounts <- replicateM n $ randomRIO @Integer (1, maxLiq)
                     bAmounts <- replicateM n $ randomRIO @Integer (1, maxLiq)
                     return $ zip5 [1..] aNames aAmounts bNames bAmounts
  where
    mkAssetNames :: IO ([Int], [Int])
    mkAssetNames = do names <- mkAssetNamesAux n []
                      return (map fst names, map snd names)
      where
        mkAssetNamesAux :: Int -> [(Int, Int)] -> IO [(Int, Int)]
        mkAssetNamesAux m acc
            |    m == 0 = return acc
            | otherwise = do (a, b) <- mkNewPair acc
                             rest <- mkAssetNamesAux (m - 1) $ (a,b) : acc
                             return $ (a, b) : rest

        mkNewPair :: [(Int, Int)] -> IO (Int, Int)
        mkNewPair acc' = do a <- randomRIO @Int (0, 2)
                            b <- randomRIO @Int (a + 1, 3)
                            if (a, b) `elem` acc'
                                then mkNewPair acc'
                                else return (a, b)

insertPools :: [Pool] -> String
insertPools = concat . map insertPool
  where
    insertPool :: Pool -> String
    insertPool (id, aName, aAmount, bName, bAmount) =
        concat [ "INSERT INTO pool VALUES ("
               , show id
               , ", '"
               , getCurrName aName
               , "', "
               , show aAmount
               , ", '"
               , getCurrName bName
               , "', "
               , show bAmount
               , ");\n"
               ]

generateTokens :: [Pool] -> [Token]
generateTokens = map mkToken
  where
    mkToken :: (Integer, Int, Integer, Int, Integer) -> (Integer, Integer)
    mkToken (id, _, aAmount, _, bAmount) =
        (id, floor $ sqrt $ fromInteger $ aAmount * bAmount)

insertTokens :: [(Integer, Integer)] -> String
insertTokens = concat . map insertToken
  where
    insertToken :: (Integer, Integer) -> String
    insertToken (id, amount) = concat [ "INSERT INTO liquidity_token VALUES ("
                                      , show id
                                      , ", "
                                      , show amount
                                      , ");\n"
                                      ]

generateOwnerships :: Int -> [Token] -> [Account] -> IO [Ownership]
generateOwnerships n tt aa =
    mkChoices n Set.empty >>= sequence . map (mkOwnership . (opts !!))
  where
    opts :: [(Token, Account)]
    opts = [(t, a) | t <- tt, a <- aa]

    poss :: Int
    poss = (length tt) * (length aa) - 1

    mkChoices :: Int -> (Set.Set Int) -> IO [Int]
    mkChoices n s |    n == 0 = return $ Set.toList s
                  | otherwise =
        do newOpt <- randomRIO (0, poss)
           if newOpt `Set.member` s
            then mkChoices n s
            else mkChoices (n - 1) $ newOpt `Set.insert` s

    mkOwnership :: (Token, Account) -> IO Ownership
    mkOwnership ((poolID, tAmount), (_, _, userID)) =
        randomRIO (1, tAmount) >>= return . (poolID, userID,)

insertOwnerships :: [Ownership] -> String
insertOwnerships = concat . map insertOwnership
  where
    insertOwnership :: Ownership -> String
    insertOwnership (pID, uID, amount) =
        concat [ "INSERT INTO liquidity_token_ownership VALUES ("
               , show pID
               , ", '"
               , uID
               , "', "
               , show amount
               , ");\n"
               ]

-- | Auxiliary.
type Account = (String, Integer, String)
type Pool = (Integer, Int, Integer, Int, Integer)
type Token = (Integer, Integer)
type Ownership = (Integer, String, Integer)

getCurrName :: Int -> String
getCurrName n |    n == 0 = "ARS"
              |    n == 1 = "EUR"
              |    n == 2 = "GBP"
              | otherwise = "USD"

maxAmount :: Integer
maxAmount = 1_000_000

maxLiq :: Integer
maxLiq = 10_000_000

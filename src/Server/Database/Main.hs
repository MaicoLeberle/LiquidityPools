{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE RecordWildCards    #-}

module Database.Main where

import Control.Exception                      (bracket)
import Control.Monad                          (void)
import Data.Aeson
import Data.ByteString.Char8                  ( ByteString
                                              , unpack
                                              )
import Data.Int                               ( Int64 )
import Data.String                            ( fromString
                                              , IsString
                                              )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import GHC.Generics                           (Generic)
import Text.Read

import qualified Business as B
import           Business          ( createUserID
                                   , initialTokens
                                   )
import           Database.Business ( fromPoolRow
                                   , fromAccountRows
                                   )
import           Database.Types    ( PoolRow
                                   , AccountRow
                                   )
import           Types


pools :: IO [Pool]
pools = bracket (connectPostgreSQL connString) close q
  where
    q :: Connection -> IO [Pool]
    q conn = query_ @PoolRow conn selectQ >>= return . map fromPoolRow

    selectQ :: Query
    selectQ = fromString $ concat [ "SELECT pool.key, "
                                      ,        "pool.asset_name_a, "
                                      ,        "pool.asset_amount_a, "
                                      ,        "pool.asset_name_b, "
                                      ,        "pool.asset_amount_b, "
                                      ,        "liquidity_token.amount "
                                      , "FROM pool INNER JOIN liquidity_token "
                                      , "ON liquidity_token.pool = pool.key"
                                      ]

createUser :: IO Password
createUser =
    B.createUserID >>= bracket (connectPostgreSQL connString) close . insert
  where
    insert :: String -> Connection -> IO Password
    insert id conn = execute_ conn (insertQ id) >> return id

    insertQ :: Password -> Query
    insertQ id = fromString $ "INSERT INTO user_id VALUES ('" ++ id ++ "')"

getAccount :: GetAccountParams -> IO GetAccountRes
getAccount GetAccountParams{gapID=pass} =
    bracket (connectPostgreSQL connString) close account
  where
    account :: Connection -> IO GetAccountRes
    account conn = do
        rows <- query @(Only Password) @AccountRow conn accountQ (Only pass)
        case fromAccountRows rows of
            Nothing  -> do userExists <- findUser
                           if userExists then return $ Right $ mkAccount pass []
                                         else return $ Left "Wrong user."
            Just res -> return $ Right res
      where
        accountQ :: Query
        accountQ = "SELECT * FROM account WHERE userID=?"

        findUser :: IO Bool
        findUser =
            query @(Only Password) @(Only String) conn findUserQ (Only pass)
                >>= return . not . null

        findUserQ :: Query
        findUserQ = "SELECT * FROM user_id WHERE key = ?"

createPool :: CreatePoolParams -> IO CreatePoolRes
createPool cpp@CreatePoolParams { cppPassword = pass
                                , cppLiq = Liq { lAssetA = aAsset
                                               , lAssetB = bAsset
                                               }
                                }
    | wrongParams = return $ Left "Cannot create pool, wrong params."
    | otherwise = bracket (connectPostgreSQL connString) close pool
  where
    wrongParams :: Bool
    wrongParams = aAmount aAsset <= 0 || aAmount bAsset <= 0

    pool :: Connection -> IO CreatePoolRes
    pool conn = do
      userExists <- doesUserExist conn pass
      if userExists
      then withTransaction conn $ rmAssetA conn
      else return $ Left "User does not exist, cannot create pool."

    rollbackTrans :: Connection -> String -> IO CreatePoolRes
    rollbackTrans conn err = rollback conn >> return (Left err)

    rmAssetA :: Connection -> IO CreatePoolRes
    rmAssetA conn = rmFunds (mkRmFundsParams pass aAsset)
                        >>= either (rollbackTrans conn) (rmAssetB conn)

    rmAssetB :: Connection -> () -> IO CreatePoolRes
    rmAssetB conn = const $ rmFunds (mkRmFundsParams pass bAsset)
                                >>= either (rollbackTrans conn) (addPool conn)

    addPool :: Connection -> () -> IO CreatePoolRes
    addPool conn = const $
        addPoolAux >>= either (rollbackTrans conn) (addTokens conn)
      where
            {-| The Integer part of the type signature for addPoolAux is the
                pool ID, not the number of initial tokens. Hence, using synonym
                CreatePoolRes instead of Either String Integer would be
                misleading.
            -}
        addPoolAux :: IO (Either String Integer)
        addPoolAux =
            execute @(Currency, Integer, Currency, Integer)
                    conn addPoolQ
                    (aName aAsset, aAmount aAsset, aName bAsset, aAmount bAsset)
                >>= findPool

        addPoolQ :: Query
        addPoolQ = fromString $ concat [ "INSERT INTO pool "
                                       , "(asset_name_A, asset_amount_A, "
                                       , "asset_name_B, asset_amount_B) "
                                       , "VALUES (?, ?, ?, ?)"
                                       ]

        findPool :: Int64 -> IO (Either String Integer)
        findPool updRows =
            if updRows == 1
            then do poolIDs <- query @(Currency, Currency) @(Only Integer)
                                     conn findPoolQ (aName aAsset, aName bAsset)
                    case poolIDs of
                        [Only pID] -> return $ Right pID
                        _          ->
                            return $ Left "Error retrieving pool identifier."
            else  return $ Left "Could not add pool to database."

        findPoolQ :: Query
        findPoolQ = fromString $ concat [ "SELECT key FROM pool WHERE "
                                        , "asset_name_A = ? "
                                        , "AND asset_name_B = ?"
                                        ]

    addTokens :: Connection -> Integer -> IO CreatePoolRes
    addTokens conn poolID =
        do updRows <- execute @(Integer, Password, Integer)
                              conn addTokensQ (poolID, pass, initialT)
           return $ if updRows == 1 then Right initialT
                                    else Left "Could not create pool."
      where
        initialT :: Integer
        initialT = B.initialTokens cpp

        addTokensQ :: Query
        addTokensQ = "INSERT INTO liquidity_token_ownership VALUES (?, ?, ?)"

addFunds :: AddFundsParams -> IO AddFundsRes
addFunds AddFundsParams{afpPassword = pass, afpAsset = a@Asset{..}} =
    bracket (connectPostgreSQL connString) close addNewAsset
  where
    addNewAsset :: Connection -> IO AddFundsRes
    addNewAsset conn =
        do userExists <- doesUserExist conn pass
           if userExists
            then execute conn insertQ (aName, aAmount, pass) >> return Nothing
            else return $ Just $
                   " User " ++ pass ++ " not found, cannot add asset " ++ show a

    insertQ :: Query
    insertQ = "INSERT INTO account VALUES (?, ?, ?)"

rmFunds :: RmFundsParams -> IO RmFundsRes
rmFunds RmFundsParams{rfpPassword = pass, rfpAsset = a@Asset{..}} =
    bracket (connectPostgreSQL connString) close rmAsset
  where
    rmAsset :: Connection -> IO RmFundsRes
    rmAsset conn = do
      assets <- query @(Currency, Password) @(Only Integer)
                      conn findAssetQ (aName, pass)
      case assets of
          []           -> return $ Left "No assets to remove."
          [Only asset] -> updateAsset conn asset aAmount
          manyAssets   -> -- ill-formed account, first fix then remove funds.
            let asset = foldr ((+) . fromOnly) 0 manyAssets
            in updateAsset conn aAmount asset

    findAssetQ :: Query
    findAssetQ = fromString $ concat [ "SELECT asset_amount FROM account "
                                     , "WHERE asset_name = ? AND userID = ?"
                                     ]

    updateAsset :: Connection -> Integer -> Integer -> IO RmFundsRes
    updateAsset conn totalAmount rmAmount =
        do if totalAmount <= rmAmount
            then return $ Left $ concat [ "Insufficient funds, cannot remove "
                                        , show rmAmount
                                        ]
            else do execute @(Integer, Currency, String)
                            conn rmQ (totalAmount - rmAmount, aName, pass)
                    return $ Right ()

    rmQ :: Query
    rmQ = fromString $ concat [ "UPDATE account SET asset_amount = ? "
                              , "WHERE asset_name = ? AND userID = ?"
                              ]



-- | Auxiliary values.
doesUserExist :: Connection -> Password -> IO Bool
doesUserExist conn pass =
    query @(Only Password) @(Only String) conn findUserQ (Only pass)
        >>= return . not . null
  where
    findUserQ :: Query
    findUserQ = "SELECT * FROM user_id WHERE key = ?"

connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"

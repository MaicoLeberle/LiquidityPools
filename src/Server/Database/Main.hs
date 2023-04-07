{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RankNTypes         #-}

module Database.Main where

import Control.Monad                          (void)
import Control.Monad.Extra                    (ifM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Exception                      (bracket)
import Data.Aeson
import Data.ByteString.Char8                  ( ByteString
                                              , unpack
                                              )
import Data.Int                               ( Int64 )
import Data.String                            ( fromString )
import Database.PostgreSQL.Simple
import GHC.Generics                           (Generic)
import Text.Read

import qualified Business          as B ( createUserID
                                        , initialTokens
                                        , newTokens
                                        , rmLiq
                                        , swap
                                        )
import           Database.Business      ( fromPoolRow
                                        , fromAccountRows
                                        )
import           Database.Types         ( PoolRow(..)
                                        , AccountRow(..)
                                        , Transaction(..)
                                        , CreatePoolParams(..)
                                        , CreatePoolRes
                                        , GetAccountParams(..)
                                        , GetAccountRes
                                        , AddFundsParams(..)
                                        , AddFundsRes
                                        , RmFundsParams(..)
                                        , RmFundsRes(..)
                                        , AddLiqParams(..)
                                        , createPoolParamsToAddLiq
                                        , Transaction
                                        , AddLiqRes
                                        , RmLiqParams(..)
                                        , RmLiqRes(..)
                                        , SwapParams(..)
                                        , SwapRes(..)
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
createPool cpp@CreatePoolParams{ cppPassword = pass
                               , cppLiq =
                                    newLiq@Liq
                                      { lAssetA = a@Asset{ aName = newAName
                                                         , aAmount = newA
                                                         }
                                      , lAssetB = b@Asset{ aName = newBName
                                                         , aAmount = newB
                                                         }
                                      }
                               }
    | wrongParams = return $ Left "Cannot create pool, wrong params."
    | otherwise =
        bracket (connectPostgreSQL connString) close (runExceptT . trans)
  where
    wrongParams :: Bool
    wrongParams = newA <= 0 || newB <= 0

    trans :: Connection -> Transaction Integer
    trans conn = do
        checkUserExists conn pass
        runAtomically conn $ do insertPool
                                addLiquidityTrans $ createPoolParamsToAddLiq cpp
      where
        insertPool :: Transaction ()
        insertPool = void $ liftedExecute conn insertPoolQ (newAName, newBName)
          where
            insertPoolQ :: Query
            insertPoolQ = fromString $ concat ["INSERT INTO pool "
                                              ,"(asset_name_A, asset_amount_A, "
                                              , "asset_name_B, asset_amount_B) "
                                              ,"VALUES (?, 0, ?, 0)"
                                              ]

addLiquidity :: AddLiqParams -> IO AddLiqRes
addLiquidity = runExceptT . addLiquidityTrans

addLiquidityTrans :: AddLiqParams -> Transaction Integer
addLiquidityTrans
    AddLiqParams { alpPassword = pass
                 , alpLiq = newLiq@Liq{ lAssetA = a@Asset{ aName = newAName
                                                         , aAmount = newA
                                                         }
                                      , lAssetB = b@Asset{ aName = newBName
                                                         , aAmount = newB
                                                         }
                                      }
                 }
    | wrongParams = except $ Left "Cannot add liquidity, wrong params."
    | otherwise =
        ExceptT $
            bracket (connectPostgreSQL connString) close (runExceptT . trans)
  where
    wrongParams :: Bool
    wrongParams = newA <= 0 || newB <= 0

    trans :: Connection -> Transaction Integer
    trans conn = do
        checkUserExists conn pass
        runAtomically conn $ do pID <- findPool newAName newBName conn
                                pool@Pool{..} <- getPoolState conn pID
                                rmAssetFromUser pass a
                                rmAssetFromUser pass b
                                newTokens <- addLiqToPool pool
                                addTokensToUser conn pass pID newTokens
                                addTokensToPool conn pID newTokens
                                return newTokens
      where
        addLiqToPool :: Pool -> Transaction Integer
        addLiqToPool pool@Pool{..} = do
            addARes <- addLiquidityToPool conn pool newAName newA
            addBRes <- addLiquidityToPool conn pool newBName newB
            if addARes == 0 || addBRes == 0
            then except $ Left "Could not add liquidity to the pool."
            else getNumberNewTokens pool

        getNumberNewTokens :: Pool -> Transaction Integer
        getNumberNewTokens Pool{..} =
            case B.newTokens oldLiq newLiq pLiqTokens of
                Nothing -> except $ Left "Error computing number of new tokens."
                Just res -> return res
          where
            oldLiq :: Liq
            oldLiq = mkLiq
                       (mkAsset (aName $ lAssetA pLiq) (aAmount $ lAssetA pLiq))
                       (mkAsset (aName $ lAssetB pLiq) (aAmount $ lAssetB pLiq))

rmLiquidity :: RmLiqParams -> IO RmLiqRes
rmLiquidity rlp@RmLiqParams{ rlpPass = pass
                           , rlpPoolID = poolID
                           , rlpTokens = tokens
                           } =
    bracket (connectPostgreSQL connString) close (runExceptT . trans)
  where
    trans :: Connection -> Transaction Liq
    trans conn = do
        checkUserExists conn pass
        runAtomically conn $ do pool@Pool{..} <- getPoolState conn poolID
                                rmTokensFromPool poolID conn tokens
                                rmTokensFromUser conn pass poolID tokens
                                liq <- rmLiqFromPool pool
                                addAssetToUser pass (lAssetA liq)
                                addAssetToUser pass (lAssetB liq)
                                return liq
      where
        mkRmLiq :: Pool -> Integer -> Integer -> Liq
        mkRmLiq Pool{..} rmA rmB =
            let a = aName $ lAssetA pLiq
                b = aName $ lAssetB pLiq
            in mkLiq (mkAsset a rmA) (mkAsset b rmB)

        rmLiqFromPool :: Pool -> Transaction Liq
        rmLiqFromPool p@Pool{pLiq = Liq{ lAssetA = Asset{aName = a}
                                       , lAssetB = Asset{aName = b}
                                       }
                            , pLiqTokens = oldTokens
                            } =
            do rmARes <- rmLiquidityFromPool conn p a rmA
               rmBRes <- rmLiquidityFromPool conn p b rmB
               if rmA == 0 || rmB == 0
               then except $ Left "Error while removing liquidity from pool."
               else return liq
          where
            rmA :: Integer
            rmA = aAmount $ lAssetA liq

            rmB :: Integer
            rmB = aAmount $ lAssetB liq

            liq :: Liq
            liq = B.rmLiq p oldTokens

addFunds :: AddFundsParams -> IO AddFundsRes
addFunds AddFundsParams{afpPassword = pass, afpAsset = a@Asset{..}} =
    bracket (connectPostgreSQL connString) close (runExceptT . addNewAsset)
  where
    addNewAsset :: Connection -> Transaction ()
    addNewAsset conn = do
        checkUserExists conn pass
        runAtomically conn $ void $ liftedExecute
                                        conn insertQ (aName, aAmount, pass)

    insertQ :: Query
    insertQ = fromString $ concat [ "INSERT INTO account"
                                  , "VALUES (?, ?, ?)"
                                  ]

rmFunds :: RmFundsParams -> IO RmFundsRes
rmFunds RmFundsParams{..} = runExceptT $ rmAssetFromUser rfpPassword rfpAsset

swap :: SwapParams -> IO SwapRes
swap SwapParams{ spPassword = pass
               , spPoolID = poolID
               , spAsset = asset@Asset{aName = swapName, aAmount = swapAmount}
               } =
    bracket (connectPostgreSQL connString) close (runExceptT . trans)
  where
    trans :: Connection -> Transaction Asset
    trans conn = do
        checkUserExists conn pass
        runAtomically conn $ do pool <- getPoolState conn poolID
                                rmAssetFromUser pass asset
                                addLiquidityToPool conn pool swapName swapAmount
                                payOut <- getPayOut pool
                                rmLiquidityFromPool
                                    conn pool (aName payOut) (aAmount payOut)
                                addAssetToUser pass payOut
                                return payOut
      where
        getPayOut :: Pool -> Transaction Asset
        getPayOut pool =
            case B.swap pool asset of
                Nothing -> except $ Left "Wrong swapping parameters."
                Just res -> return res

-- | Auxiliary values.
getPoolState :: Connection -> Integer -> Transaction Pool
getPoolState conn poolID = do
    res <- liftedQuery @(Only Integer)
                       @(Currency, Integer, Currency, Integer, Integer)
                       conn getNumberNewTokensQ (Only poolID)
    case res of
        [(aName, oldA, bName, oldB, oldTokens)] ->
          return $ mkPool poolID
                          (mkLiq (mkAsset aName oldA)
                                 (mkAsset bName oldB))
                          oldTokens
        _ -> except $ Left "Error retrieving previous liquidity."
  where
    getNumberNewTokensQ :: Query
    getNumberNewTokensQ =
        fromString $ concat
            [ "SELECT asset_name_A, asset_amount_A, "
            ,        "asset_name_B, asset_amount_B, amount"
            , "FROM pool"
            , "INNER JOIN liquidity_token ON key = pool "
            , "WHERE key = ?"
            ]

addLiquidityToPool :: Connection
                   -> Pool
                   -> Currency
                   -> Integer
                   -> Transaction Int64
addLiquidityToPool conn
                   Pool{ pLiq = Liq{ lAssetA = Asset{aName = a, aAmount = oldA}
                                   , lAssetB = Asset{aName = b, aAmount = oldB}
                                   }
                       , ..
                       }
                   inputName
                   inputAmount
    | inputName == a && oldA + inputAmount >= 0 =
        liftedExecute @(Currency, Integer, Integer)
                      conn addLiquidityToPoolQ (inputName, inputAmount, pID)
    | inputName == a && oldA + inputAmount >= 0 =
        liftedExecute @(Currency, Integer, Integer)
                      conn addLiquidityToPoolQ (inputName, inputAmount, pID)
    | otherwise = except $ Left "Error while udpating liquidity in pool."
  where
    addLiquidityToPoolQ :: Query
    addLiquidityToPoolQ = fromString $ concat [ "UPDATE liquidity_token "
                                              , "SET amount = amount + ? "
                                              , "WHERE pool = ?"
                                              ]

rmLiquidityFromPool :: Connection
                    -> Pool
                    -> Currency
                    -> Integer
                    -> Transaction Int64
rmLiquidityFromPool c p n = addLiquidityToPool c p n . negate


addAssetToUser :: Password -> Asset -> Transaction ()
addAssetToUser pass Asset{..} = ExceptT $ bracket (connectPostgreSQL connString)
                                                  close
                                                  (runExceptT . trans)
  where
    trans :: Connection -> Transaction ()
    trans conn = findAsset >>= updateAsset
      where
        findAsset :: Transaction Integer
        findAsset = do
            assets <- liftedQuery @(Currency, Password) @(Only Integer)
                                  conn findAssetQ (aName, pass)
            case assets of
                []           -> except $ Left "No assets to remove."
                [Only asset] -> return asset
                manyAssets   -> except $ Left "Ill-formed account, fix manually"

        findAssetQ :: Query
        findAssetQ =
            fromString $ concat [ "SELECT asset_amount "
                                , "FROM account "
                                , "WHERE asset_name = ? AND userID = ?"
                                ]

        updateAsset :: Integer -> Transaction ()
        updateAsset totalAmount
          | totalAmount + aAmount < 0 =
              except $ Left $ concat [ "Insufficient funds, cannot remove "
                                     , show (abs aAmount)
                                     , " from a total of "
                                     , show totalAmount
                                     ]
          | otherwise =
              void $ liftedExecute @(Integer, Currency, String)
                                   conn updateAQ (aAmount, aName, pass)

        updateAQ :: Query
        updateAQ = fromString $ concat [ "UPDATE account "
                                       , "SET asset_amount = asset_amount + ? "
                                       , "WHERE asset_name = ? AND userID = ?"
                                       ]

rmAssetFromUser :: Password -> Asset -> Transaction ()
rmAssetFromUser pass a@Asset{aAmount = amount} =
    addAssetToUser pass a{aAmount = -amount}

connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"

checkUserExists :: Connection -> Password -> Transaction ()
checkUserExists conn pass = ExceptT $
    ifM (doesUserExist conn pass)
        (return $ Right ())
        (return $ Left $ "User not found.")

doesUserExist :: Connection -> Password -> IO Bool
doesUserExist conn pass =
    query @(Only Password) @(Only String) conn findUserQ (Only pass)
        >>= return . not . null
  where
    findUserQ :: Query
    findUserQ = fromString $ concat [ "SELECT * "
                                    , "FROM user_id "
                                    , "WHERE key = ?"
                                    ]

findPool :: Currency -> Currency -> Connection -> Transaction Integer
findPool aCurrency bCurrency conn = do
    pools <- liftIO (query @(Currency, Currency) @(Only Integer)
                           conn findPoolQ (aCurrency, bCurrency))
    case pools of
      [Only pID] -> return pID
      _          -> except $ Left "Error retrieving pool identifier"
  where
    findPoolQ :: Query
    findPoolQ =
        fromString $ concat [ "SELECT key "
                            , "FROM pool "
                            , "WHERE asset_name_A = ? AND asset_name_B = ?"
                            ]

addTokensToPool :: Connection
                -> Integer
                -> Integer
                -> Transaction ()
addTokensToPool conn poolID tokens = do
    query <- selectQuery
    updRows <- liftedExecute @(Integer, Integer) conn query (tokens, poolID)
    if updRows == 1
    then return ()
    else except $ Left "Could not remove liquidity tokens from pool."
  where
    selectQuery :: Transaction Query
    selectQuery = do
        pools <- liftedQuery @(Only Integer) @(Only Integer)
                             conn poolExistsQ (Only poolID)
        case pools of
            []  -> return initialQ
            [_] -> return inductiveQ
            _   -> except $ Left "Database corrupted."

    poolExistsQ :: Query
    poolExistsQ = fromString $ concat [ "SELECT pool "
                                      , "FROM liquidity_token "
                                      , "WHERE pool = ?"
                                      ]

    initialQ :: Query
    initialQ =
        fromString $ concat [ "INSERT INTO liquidity_token (amount, pool) "
                            , "VALUES (?, ?)"
                            ]

    inductiveQ :: Query
    inductiveQ = fromString $ concat [ "UPDATE liquidity_token "
                                     , "SET amount = amount - ? "
                                     , "WHERE pool = ?"
                                     ]

rmTokensFromPool :: Integer
                 -> Connection
                 -> Integer
                 -> Transaction ()
rmTokensFromPool poolID conn tokens = do
    checkPoolExists
    updRows <- liftedExecute @(Integer, Integer)
                             conn rmTokensFromPoolQ (tokens, poolID)
    if updRows == 1
    then return ()
    else except $ Left "Could not remove liquidity tokens from pool."
  where
    checkPoolExists :: Transaction ()
    checkPoolExists = do
        pools <- liftedQuery @(Only Integer) @(Only Integer)
                             conn poolExistsQ (Only poolID)
        case pools of
            []  -> except $ Left
                      "Pool not found, could not remove liquidity tokens."
            [_] -> return ()
            _   -> except $ Left "Database corrupted."

    poolExistsQ :: Query
    poolExistsQ = fromString $ concat [ "SELECT pool "
                                      , "FROM liquidity_token "
                                      , "WHERE pool = ?"
                                      ]

    rmTokensFromPoolQ :: Query
    rmTokensFromPoolQ = fromString $ concat [ "UPDATE liquidity_token "
                                            , "SET amount = amount - ? "
                                            , "WHERE pool = ?"
                                            ]

addTokensToUser :: Connection
                -> Password
                -> Integer
                -> Integer
                -> Transaction Integer
addTokensToUser conn pass poolID tokens = do
    updRows <- liftedExecute @(Integer, Integer, Password)
                             conn addTokensToUserQ (tokens, poolID, pass)
    if updRows == 1
    then return tokens
    else except $ Left "Could not update liquidity tokens for user."
  where
    addTokensToUserQ :: Query
    addTokensToUserQ =
        fromString $ concat
            [ "UPDATE liquidity_token_ownership "
            , "SET liquidity_token_amount = liquidity_token_amount + ? "
            , "WHERE pool = ? AND userID = ?"
            ]

rmTokensFromUser :: Connection
                 -> Password
                 -> Integer
                 -> Integer
                 -> Transaction Integer
rmTokensFromUser pass p conn tokens = addTokensToUser pass p conn (-tokens)

liftedExecute :: ToRow q => Connection -> Query -> q -> Transaction Int64
liftedExecute conn q = liftIO . execute conn q

liftedQuery :: (ToRow q, FromRow r)
            => Connection
            -> Query
            -> q
            -> Transaction [r]
liftedQuery conn q = liftIO . query conn q

runAtomically :: Connection -> Transaction a -> Transaction a
runAtomically conn = ExceptT . withTransaction conn . runExceptT

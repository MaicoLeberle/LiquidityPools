{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RankNTypes         #-}

module Database.Main where

import Control.Monad.Extra                    (ifM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
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
        checkUserExists
        runAtomically $ do rmAsset pass a
                           rmAsset pass b
                           pID <- getPool
                           res <- addL pID
                           newTokens <- getNumberNewTokens pID
                           addTokens pass pID conn newTokens
                           except $ Right newTokens
      where
        checkUserExists :: Transaction ()
        checkUserExists = ExceptT $
            ifM (doesUserExist conn pass)
                (return $ Right ())
                (return $ Left
                    "User does not exist, cannot add liquidity to pool.")

        runAtomically :: Transaction Integer -> Transaction Integer
        runAtomically = ExceptT . withTransaction conn . runExceptT

        getPool :: Transaction Integer
        getPool = findPool newAName newBName conn

        addL :: Integer -> Transaction ()
        addL poolID = do
            updRows <-
                liftedExecute @(Integer, Integer, Integer, Currency, Currency)
                              conn
                              addLiquidityQ
                              (newA, newB, poolID, newAName, newBName)
            if updRows == 1
            then except $ Right ()
            else except $ Left "Could not add liquidity to the pool."
          where
            addLiquidityQ :: Query
            addLiquidityQ =
                fromString $ concat
                    [ "UPDATE account SET asset_amount_A = asset_amount_A + ?, "
                    , "asset_amount_B = asset_amount_B + ? WHERE key = ? "
                    , "AND asset_name_A = ? AND asset_name_B = ?"
                    ]

        getNumberNewTokens :: Integer -> Transaction Integer
        getNumberNewTokens pID = do
            res <- liftedQuery @(Only Integer) @(Integer, Integer, Integer)
                               conn getNumberNewTokensQ (Only pID)
            case res of
                [(oldA, oldB, oldTokens)] ->
                    let oldLiq = mkLiq (mkAsset newAName oldA)
                                       (mkAsset newBName oldB)
                    in  return $ B.initialTokens cpp
                _ -> except $ Left "Error retrieving previous liquidity."
          where
            getNumberNewTokensQ :: Query
            getNumberNewTokensQ =
                fromString $ concat [ "SELECT asset_amount_A, asset_amount_B, "
                                    , "amount "
                                    , "FROM pool INNER JOIN liquidity_token "
                                    , "ON key = pool WHERE key = ?"
                                    ]

addLiq :: AddLiqParams -> IO AddLiqRes
addLiq AddLiqParams { alpPassword = pass
                    , alpLiq = newLiq@Liq{ lAssetA = a@Asset{ aName = newAName
                                                            , aAmount = newA
                                                            }
                                         , lAssetB = b@Asset{ aName = newBName
                                                            , aAmount = newB
                                                            }
                                         }
                    }
    | wrongParams = return $ Left "Cannot add liquidity, wrong params."
    | otherwise =
        bracket (connectPostgreSQL connString) close (runExceptT . trans)
  where
    wrongParams :: Bool
    wrongParams = newA <= 0 || newB <= 0

    trans :: Connection -> Transaction Integer
    trans conn = do
        checkUserExists
        runAtomically $ do rmAsset pass a
                           rmAsset pass b
                           pID <- getPool
                           res <- addL pID
                           newTokens <- getNumberNewTokens pID
                           addTokens pass pID conn newTokens
                           except $ Right newTokens
      where
        checkUserExists :: Transaction ()
        checkUserExists = ExceptT $
            ifM (doesUserExist conn pass)
                (return $ Right ())
                (return $ Left
                    "User does not exist, cannot add liquidity to pool.")

        runAtomically :: Transaction Integer -> Transaction Integer
        runAtomically = ExceptT . withTransaction conn . runExceptT

        getPool :: Transaction Integer
        getPool = findPool newAName newBName conn

        addL :: Integer -> Transaction ()
        addL poolID = do
            updRows <-
                liftedExecute @(Integer, Integer, Integer, Currency, Currency)
                              conn
                              addLiquidityQ
                              (newA, newB, poolID, newAName, newBName)
            if updRows == 1
            then except $ Right ()
            else except $ Left "Could not add liquidity to the pool."
          where
            addLiquidityQ :: Query
            addLiquidityQ =
                fromString $ concat
                    [ "UPDATE account SET asset_amount_A = asset_amount_A + ?, "
                    , "asset_amount_B = asset_amount_B + ? WHERE key = ? "
                    , "AND asset_name_A = ? AND asset_name_B = ?"
                    ]

        getNumberNewTokens :: Integer -> Transaction Integer
        getNumberNewTokens pID = do
            res <- liftedQuery @(Only Integer) @(Integer, Integer, Integer)
                               conn getNumberNewTokensQ (Only pID)
            case res of
                [(oldA, oldB, oldTokens)] ->
                    let oldLiq = mkLiq (mkAsset newAName oldA)
                                       (mkAsset newBName oldB)
                    in  case B.newTokens oldLiq newLiq oldTokens of
                          Nothing -> except $ Left
                                        "Wrong call to compute new tokens."
                          Just res -> return res
                _ -> except $ Left "Error retrieving previous liquidity."
          where
            getNumberNewTokensQ :: Query
            getNumberNewTokensQ =
                fromString $ concat [ "SELECT asset_amount_A, asset_amount_B, "
                                    , "amount "
                                    , "FROM pool INNER JOIN liquidity_token "
                                    , "ON key = pool WHERE key = ?"
                                    ]

addFunds :: AddFundsParams -> IO AddFundsRes
addFunds AddFundsParams{afpPassword = pass, afpAsset = a@Asset{..}} =
    bracket (connectPostgreSQL connString) close addNewAsset
  where
    addNewAsset :: Connection -> IO AddFundsRes
    addNewAsset conn =
        do userExists <- doesUserExist conn pass
           if userExists
            then do execute conn insertQ (aName, aAmount, pass)
                    return $ Right ()
            else return $ Left $
                   " User " ++ pass ++ " not found, cannot add asset " ++ show a

    insertQ :: Query
    insertQ = "INSERT INTO account VALUES (?, ?, ?)"

rmFunds :: RmFundsParams -> IO RmFundsRes
rmFunds RmFundsParams{..} = runExceptT $ rmAsset rfpPassword rfpAsset


-- | Auxiliary values.
rmAsset :: Password -> Asset -> Transaction ()
rmAsset pass Asset{..} =
    ExceptT $ bracket (connectPostgreSQL connString) close (runExceptT . rmAsset)
  where
    rmAsset :: Connection -> Transaction ()
    rmAsset conn = findAsset conn >>= updateAsset conn aAmount

    findAsset :: Connection -> Transaction Integer
    findAsset conn = do
        assets <- liftedQuery @(Currency, Password) @(Only Integer)
                              conn findAssetQ (aName, pass)
        case assets of
            []           -> except $ Left "No assets to remove."
            [Only asset] -> return asset
            manyAssets   -> except $ Left "Ill-formed account, fix manually."
      where
        findAssetQ :: Query
        findAssetQ =
            fromString $ concat [ "SELECT asset_amount FROM account "
                                , "WHERE asset_name = ? AND userID = ?"
                                ]

    updateAsset :: Connection -> Integer -> Integer -> Transaction ()
    updateAsset conn totalAmount rmAmount
      | totalAmount <= rmAmount =
          except $ Left $ concat [ "Insufficient funds, cannot remove "
                                 , show rmAmount
                                 , " from a total of "
                                 , show totalAmount
                                 ]
      | otherwise = do liftedExecute @(Integer, Currency, String) conn rmQ
                                     (totalAmount - rmAmount, aName, pass)
                       return ()

    rmQ :: Query
    rmQ = fromString $ concat [ "UPDATE account SET asset_amount = ? "
                              , "WHERE asset_name = ? AND userID = ?"
                              ]

connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"

doesUserExist :: Connection -> Password -> IO Bool
doesUserExist conn pass =
    query @(Only Password) @(Only String) conn findUserQ (Only pass)
        >>= return . not . null
  where
    findUserQ :: Query
    findUserQ = "SELECT * FROM user_id WHERE key = ?"

findPool :: Currency -> Currency -> Connection -> Transaction Integer
findPool aCurrency bCurrency conn = do
    pools <- liftIO (query @(Currency, Currency) @(Only Integer)
                           conn findPoolQ (aCurrency, bCurrency))
    case pools of
      [Only pID] -> except $ Right pID
      _          -> except $ Left "Error retrieving pool identifier"

findPoolQ :: Query
findPoolQ = fromString $ concat [ "SELECT key FROM pool WHERE "
                                , "asset_name_A = ? "
                                , "AND asset_name_B = ?"
                                ]

addTokens :: Password -> Integer -> Connection -> Integer -> Transaction Integer
addTokens pass poolID conn tokens = do
    updRows <- liftIO $ execute @(Integer, Password, Integer)
                                conn addTokensQ (poolID, pass, tokens)
    if updRows == 1
    then except $ Right tokens
    else except $ Left "Could not create pool."
  where
    addTokensQ :: Query
    addTokensQ = "INSERT INTO liquidity_token_ownership VALUES (?, ?, ?)"

liftedExecute :: ToRow q => Connection -> Query -> q -> Transaction Int64
liftedExecute conn q = liftIO . execute conn q

liftedQuery :: (ToRow q, FromRow r)
            => Connection
            -> Query
            -> q
            -> Transaction [r]
liftedQuery conn q = liftIO . query conn q
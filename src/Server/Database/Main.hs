{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE RecordWildCards    #-}

module Database.Main where

import Control.Exception                    (bracket)
import Control.Monad                        (void)
import Data.Aeson
import Data.ByteString.Char8                ( ByteString
                                            , unpack
                                            )
import Data.Int                             ( Int64 )
import Data.String                          ( fromString
                                            , IsString
                                            )
import Database.PostgreSQL.Simple
import GHC.Generics                         (Generic)
import Text.Read

import Business          ( createUserID
                         )
import Database.Business ( fromPoolRow
                         , fromAccountRows
                         )
import Database.Types    ( PoolRow
                         , AccountRow
                         )
import Types


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

insertUser :: IO Password
insertUser =
    createUserID >>= bracket (connectPostgreSQL connString) close . insert
  where
    insert :: String -> Connection -> IO Password
    insert id conn = execute_ conn (insertQ id) >> return id

    insertQ :: Password -> Query
    insertQ id = fromString $ "INSERT INTO user_id VALUES ('" ++ id ++ "')"

getAccount :: Password -> IO (Either String Account)
getAccount pass = bracket (connectPostgreSQL connString) close account
  where
    account :: Connection -> IO (Either String Account)
    account conn = do
        rows <- query @(Only Password) @AccountRow conn accountQ (Only pass)
        case fromAccountRows rows of
            Nothing -> do userExists <- findUser
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

addFunds :: Password -> Asset -> IO (Maybe String)
addFunds pass a@Asset{..} =
    bracket (connectPostgreSQL connString) close addNewAsset
  where
    addNewAsset :: Connection -> IO (Maybe String)
    addNewAsset conn =
      do user <-
            query @(Only Password) @(Only String) conn findUserQ (Only pass)
         if null user
          then return $ Just $ "Unexistent user, cannot add asset " ++ show a
          else execute conn insertQ (aName, aAmount, pass) >> return Nothing

    findUserQ :: Query
    findUserQ = "SELECT * FROM user_id WHERE key = ?"

    insertQ :: Query
    insertQ = "INSERT INTO account VALUES (?, ?, ?)"

rmFunds :: Password -> Asset -> IO (Maybe String)
rmFunds pass a@Asset{..} = bracket (connectPostgreSQL connString) close rmAsset
  where
    rmAsset :: Connection -> IO (Maybe String)
    rmAsset conn = do
      assets <- query @(Currency, Password) @(Only Integer)
                      conn findAssetQ (aName, pass)
      case assets of
          []           -> return $ Just "No assets to remove."
          [Only asset] -> updateAsset conn asset aAmount
          manyAssets   -> -- ill-formed account, first fix then remove funds.
            let asset = foldr ((+) . fromOnly) 0 manyAssets
            in updateAsset conn aAmount asset

    findAssetQ :: Query
    findAssetQ = fromString $ concat [ "SELECT asset_amount FROM account "
                                     , "WHERE asset_name = ? AND userID = ?"
                                     ]

    updateAsset :: Connection -> Integer -> Integer -> IO (Maybe String)
    updateAsset conn totalAmount rmAmount =
        do if totalAmount <= rmAmount
            then return $ Just $
                    "Insufficient funds, cannot remove " ++ show rmAmount
            else do execute @(Integer, Currency, String)
                            conn rmQ (totalAmount - rmAmount, aName, pass)
                    return Nothing

    rmQ :: Query
    rmQ = fromString $ concat [ "UPDATE account SET asset_amount = ? "
                              , "WHERE asset_name = ? AND userID = ?"
                              ]



-- | Auxiliary values.
connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"

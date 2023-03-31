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
    q conn = query_ @PoolRow conn selectQuery >>= return . map fromPoolRow

    selectQuery :: Query
    selectQuery = fromString $ concat [ "SELECT pool.key, "
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
    insert id conn = execute_ conn (insertQuery id) >> return id

    insertQuery :: Password -> Query
    insertQuery id = fromString $ "INSERT INTO user_id VALUES ('" ++ id ++ "')"

getAccount :: Password -> IO (Either String Account)
getAccount pass = bracket (connectPostgreSQL connString) close account
  where
    account :: Connection -> IO (Either String Account)
    account conn = do
        rows <- query @(Only Password) @AccountRow conn accountQuery (Only pass)
        case fromAccountRows rows of
            Nothing -> do userExists <- findUser
                          if userExists then return $ Right $ mkAccount pass []
                                        else return $ Left "Wrong user."
            Just res -> return $ Right res
      where
        accountQuery :: Query
        accountQuery = "SELECT * FROM account WHERE userID=?"

        findUser :: IO Bool
        findUser =
            query @(Only Password) @(Only String) conn findUserQuery (Only pass)
                >>= return . not . null

        findUserQuery :: Query
        findUserQuery = "SELECT * FROM user_id WHERE key = ?"

addFunds :: Password -> Asset -> IO (Maybe String)
addFunds pass a@Asset{..} =
    bracket (connectPostgreSQL connString) close addNewAsset
  where
    addNewAsset :: Connection -> IO (Maybe String)
    addNewAsset conn =
      do user <-
            query @(Only Password) @(Only String) conn findUserQuery (Only pass)
         if null user
          then return $ Just $ "Unexistent user, cannot add asset " ++ show a
          else execute conn insertQuery (aName, aAmount, pass) >> return Nothing

    findUserQuery :: Query
    findUserQuery = "SELECT * FROM user_id WHERE key = ?"

    insertQuery :: Query
    insertQuery = "INSERT INTO account VALUES (?, ?, ?)"



-- | Auxiliary values.
connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"

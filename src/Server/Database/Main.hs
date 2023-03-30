{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

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

import Business       ( createUserID
                      )
import Database.Types ( PoolRow
                      , fromPoolRow
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


-- | Auxiliary values.
connString :: ByteString
connString = "host=localhost dbname=liquiditypools password=1234"
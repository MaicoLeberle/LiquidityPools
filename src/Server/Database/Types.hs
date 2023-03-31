{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Types
    ( PoolRow
    , AccountRow
    ) where

import Data.Aeson
import Data.ByteString.Char8                (unpack)
import Data.List
import Data.String                          (IsString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import Text.Read (readMaybe)

import Business
import Types


type PoolRow = ( Integer   -- pool ID
               , Currency  -- name of asset A
               , Integer   -- amount of asset A
               , Currency  -- name of asset B
               , Integer   -- amount of asset B
               , Integer   -- total number of liquidity tokens
               )

instance FromField Currency where
    fromField f Nothing = returnError UnexpectedNull f "Unexpected null."
    fromField f (Just bs) =
        case toMaybeCurrency bs of
            Nothing -> returnError ConversionFailed f $ "Conversion failed."
            Just res -> return res
      where
        toMaybeCurrency :: (Eq p, IsString p) => p -> Maybe Currency
        toMaybeCurrency "ARS" = Just ARS
        toMaybeCurrency "EUR" = Just EUR
        toMaybeCurrency "GBP" = Just GBP
        toMaybeCurrency "USD" = Just USD
        toMaybeCurrency     _ = Nothing

type AccountRow = ( Currency -- Asset name
                  , Integer  -- Asset amount
                  , String   -- User who owns this asset.
                  )

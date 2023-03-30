{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module Database.Types
    ( PoolRow
    , fromPoolRow
    ) where

import Data.Aeson
import Text.Read (readMaybe)
import Data.ByteString.Char8                (unpack)
import Data.String                          (IsString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import Types

import Business


type PoolRow = ( Integer   -- pool ID
               , Currency  -- name of asset A
               , Integer   -- amount of asset A
               , Currency  -- name of asset B
               , Integer   -- amount of asset B
               , Integer   -- total number of liquidity tokens
               )

fromPoolRow :: PoolRow -> Pool
fromPoolRow (pID, aAsset, aAmount, bAsset, bAmount, tokens) =
    mkPool pID (mkLiq (mkAsset aAsset aAmount) (mkAsset bAsset bAmount)) tokens

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

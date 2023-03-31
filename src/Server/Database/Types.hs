{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Types
    ( PoolRow
    , fromPoolRow
    , AccountRow
    , fromAccountRows
    ) where

import Data.Aeson
import Data.ByteString.Char8                (unpack)
import Data.List
import Data.Map                             as M ( insertWith
                                                 , toList
                                                 )
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

type AccountRow = ( Currency -- Asset name
                  , Integer  -- Asset amount
                  , String   -- User who owns this asset.
                  )

fromAccountRows :: [AccountRow] -> Maybe Account
fromAccountRows                      [] = Nothing
fromAccountRows rows@((_, _, pass) : _) = Just $ accountFromRows rows
  where
        {-  Note that the foldr part is required in the absence of the invariant
            saying that addFunds doesn't add a new entry with asset name a and
            userID u to the account table if an entry for those same asset name
            and userID already exists.
        -}
    accountFromRows :: [AccountRow] -> Account
    accountFromRows =
        mkAccount pass . map (uncurry mkAsset)
                       . M.toList
                       . foldr (\(c, a, _) s -> M.insertWith (+) c a s) mempty

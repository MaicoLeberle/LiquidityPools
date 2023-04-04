{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Business
    ( fromPoolRow
    , fromAccountRows
    ) where

import Data.Aeson
import Data.ByteString.Char8                ( ByteString
                                            , pack
                                            , unpack
                                            )
import Data.List
import Data.Map                             as M ( insertWith
                                                 , toList
                                                 )
import Data.String                          (IsString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Text.Read                            (readMaybe)

import Business
import Database.Types ( PoolRow
                      , AccountRow
                      )
import Types


instance FromField Currency where
    fromField :: Field -> Maybe ByteString -> Conversion Currency
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

instance ToField Currency where
    toField :: Currency -> Action
    toField = Escape . pack . show

fromPoolRow :: PoolRow -> Pool
fromPoolRow (pID, aAsset, aAmount, bAsset, bAmount, tokens) =
    mkPool pID (mkLiq (mkAsset aAsset aAmount) (mkAsset bAsset bAmount)) tokens

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

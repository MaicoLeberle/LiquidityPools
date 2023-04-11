{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Types
    ( Pool(..)
    , mkPool
    , Liq(..)
    , mkLiq
    , Asset(..)
    , mkAsset
    , Currency(..)
    , Account(..)
    , mkAccount
    , Password
    ) where

import Data.Aeson
import GHC.Generics


data Pool = Pool
    { pID        :: Integer
    , pLiq       :: Liq
    , pLiqTokens :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON)

mkPool :: Integer -> Liq -> Integer -> Pool
mkPool = Pool

data Liq = Liq
    { lAssetA :: Asset
    , lAssetB :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkLiq :: Asset -> Asset -> Liq
mkLiq = Liq

data Asset = Asset
    { aName   :: Currency
    , aAmount :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAsset :: Currency -> Integer -> Asset
mkAsset = Asset

data Currency =
      ARS
    | EUR
    | GBP
    | USD
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Account = Account
    { aUserID :: Password
    , aAssets :: [Asset]
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccount :: Password -> [Asset] -> Account
mkAccount = Account

type Password = String

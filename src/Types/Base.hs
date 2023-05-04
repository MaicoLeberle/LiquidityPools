{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Base
    ( Pool(..)
    , mkPool
    , Liq(..)
    , mkLiq
    , Asset(..)
    , mkAsset
    , Currency(..)
    , fieldNames
    , Account(..)
    , mkAccount
    , Password
    ) where

import Data.Aeson
import GHC.Generics
import Language.Haskell.TH


data Pool = Pool
    { pID        :: Integer
    , pLiq       :: Liq
    , pLiqTokens :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | fieldNames is used to list Currency data constructors in client side app.
fieldNames :: Name -> ExpQ
fieldNames t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t
    let ns = map names constructors
    [| ns |]
  where
    names :: Con -> String
    names (NormalC name _) = nameBase name

data Account = Account
    { aUserID :: Password
    , aAssets :: [Asset]
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccount :: Password -> [Asset] -> Account
mkAccount = Account

type Password = String

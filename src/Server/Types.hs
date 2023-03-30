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
    , SubscribeRes(..)
        -- Create pool.
    , CreatePoolParams(..)
    , CreatePoolRes(..)
    , mkCreatePoolRes
        -- Retrieve account state.
    , AccountStateParams(..)
    , AccountStateRes(..)
    , mkAccountStateRes
        -- Add funds to an account.
    , AddFundsParams(..)
    , mkAddFundsParams
    , AddFundsRes(..)
    , mkAddFundsRes
        -- Remove funds from an account.
    , RmFundsParams(..)
    , mkRmFundsParams
    , RmFundsRes(..)
        -- Add liquidity from an account to a pool.
    , AddLiqParams(..)
    , AddLiqRes(..)
    , mkAddLiqRes
        -- Remove liquidity from a pool to an account.
    , RmLiqParams(..)
    , RmLiqRes(..)
    , mkRmLiqRes
        -- Swap liquidity at a pool.
    , SwapParams(..)
    , SwapRes(..)
    , mkSwapRes
    ) where

import Data.Aeson
import Data.String
import Database.PostgreSQL.Simple.FromField
import GHC.Generics

import Servant


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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Account = Account
    { aUserID :: Password
    , aAssets :: [Asset]
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccount :: Password -> [Asset] -> Account
mkAccount = Account

type Password = String

type SubscribeRes = String

newtype AccountStateParams = AccountStateParams { aspID :: String }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype AccountStateRes = AccountStateRes { asrFunds :: [Asset] }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccountStateRes :: [Asset] -> AccountStateRes
mkAccountStateRes = AccountStateRes

newtype CreatePoolParams = CreatePoolParams {cppLiq :: Liq}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Represents number of LP tokens assigned to the pool creator
newtype CreatePoolRes = CreatePoolRes { cprLiqTokens :: Integer }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkCreatePoolRes :: Integer -> CreatePoolRes
mkCreatePoolRes = CreatePoolRes

newtype AddFundsParams = AddFundsParams { afpFunds :: [Asset] }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAddFundsParams :: [Asset] -> AddFundsParams
mkAddFundsParams = AddFundsParams

newtype AddFundsRes = AddFundsRes { afrAccount :: Account }
  deriving (Eq, Show, Generic, ToJSON)

mkAddFundsRes :: Account -> AddFundsRes
mkAddFundsRes = AddFundsRes

newtype RmFundsParams = RmFundsParams { rfpFunds :: [Asset] }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkRmFundsParams :: [Asset] -> RmFundsParams
mkRmFundsParams = RmFundsParams

newtype RmFundsRes = RmFundsRes { rfrAccount :: Account }
  deriving (Eq, Show, Generic, ToJSON)

data AddLiqParams = AddLiqParams
    { alpLiq     :: Liq
    , alpAccount :: Account
    }
  deriving (Eq, Show, Generic, FromJSON)

data AddLiqRes = AddLiqRes
    { alrPool    :: Pool
    , alrAccount :: Account
    }
  deriving (Eq, Show, Generic, ToJSON)

mkAddLiqRes :: Pool -> Account -> AddLiqRes
mkAddLiqRes = AddLiqRes

data RmLiqParams = RmLiqParams
    { rlpLiqTokens :: Integer
    , rlpAccount  :: Account
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RmLiqRes = RmLiqRes
    {   -- Pool after this operation has taken place.
      rmrPool    :: Pool
        -- Account performing the operation gets updated with redeemed liquidity.
    , rmrAccount :: Account
    }
  deriving (Eq, Show, Generic, ToJSON)

mkRmLiqRes :: Pool -> Account -> RmLiqRes
mkRmLiqRes = RmLiqRes

data SwapParams = SwapParams
    { spAsset   :: Asset
    , spAccount :: Account
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SwapRes = SwapRes
    { srPool    :: Pool
    , srAsset   :: Asset
    , srAccount :: Account
    }
  deriving (Eq, Show, Generic, ToJSON)

mkSwapRes :: Pool -> Asset -> Account -> SwapRes
mkSwapRes = SwapRes

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
    , GetAccountParams(..)
    , GetAccountRes
        -- Add funds to an account.
    , AddFundsParams(..)
    , mkAddFundsParams
    , AddFundsRes
        -- Remove funds from an account.
    , RmFundsParams(..)
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
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Account = Account
    { aUserID :: Password
    , aAssets :: [Asset]
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccount :: Password -> [Asset] -> Account
mkAccount = Account

type Password = String

type SubscribeRes = String

newtype GetAccountParams = GetAccountParams { gapID :: String }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type GetAccountRes = Either String Account

newtype CreatePoolParams = CreatePoolParams {cppLiq :: Liq}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Represents number of LP tokens assigned to the pool creator
newtype CreatePoolRes = CreatePoolRes { cprLiqTokens :: Integer }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkCreatePoolRes :: Integer -> CreatePoolRes
mkCreatePoolRes = CreatePoolRes

data AddFundsParams = AddFundsParams
    { afpPassword :: Password
    , afpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAddFundsParams :: Password -> Asset -> AddFundsParams
mkAddFundsParams = AddFundsParams

type AddFundsRes = Maybe String

data RmFundsParams = RmFundsParams
    { rfpPassword :: Password
    , rfpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type RmFundsRes = Maybe String

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
        -- Account performing the operation s updated with redeemed liquidity.
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

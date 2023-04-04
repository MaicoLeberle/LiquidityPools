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
    , CreatePoolRes
        -- Retrieve account state.
    , GetAccountParams(..)
    , GetAccountRes
        -- Add funds to an account.
    , AddFundsParams(..)
    , mkAddFundsParams
    , AddFundsRes
        -- Remove funds from an account.
    , RmFundsParams(..)
    , mkRmFundsParams
    , RmFundsRes(..)
        -- Add liquidity from an account to a pool.
    , AddLiqParams(..)
    , Transaction
    , AddLiqRes
        -- Remove liquidity from a pool to an account.
    , RmLiqParams(..)
    , RmLiqRes(..)
    , mkRmLiqRes
        -- Swap liquidity at a pool.
    , SwapParams(..)
    , SwapRes(..)
    , mkSwapRes
    ) where

import Control.Monad.Trans.Except
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

{-| Type Transaction allows for a compact and maintainable implementation of
    error-prone, multi-query sessions with the database backend.
-}
type Transaction a = ExceptT String IO a

-- | account endpoint.
newtype GetAccountParams = GetAccountParams { gapID :: String }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type GetAccountRes = Either String Account

-- | createPool endpoint.
data CreatePoolParams = CreatePoolParams
    { cppPassword :: Password
    , cppLiq :: Liq
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Represents number of LP tokens assigned to the pool creator
type CreatePoolRes = Either String Integer

-- | addFunds endpoint.
data AddFundsParams = AddFundsParams
    { afpPassword :: Password
    , afpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAddFundsParams :: Password -> Asset -> AddFundsParams
mkAddFundsParams = AddFundsParams

type AddFundsRes = Either String ()

-- | rmFunds endpoint.
data RmFundsParams = RmFundsParams
    { rfpPassword :: Password
    , rfpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkRmFundsParams :: Password -> Asset -> RmFundsParams
mkRmFundsParams = RmFundsParams

type RmFundsRes = Either String ()

-- | addLiquidity endpoint.
data AddLiqParams = AddLiqParams
    { alpPassword :: Password
    , alpLiq     :: Liq
    }
  deriving (Eq, Show, Generic, FromJSON)

type AddLiqRes = Either String Integer

-- | rmLiquidity endpoint.
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

-- | swap endpoint.
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

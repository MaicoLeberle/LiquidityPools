{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Types
    ( Pool(..)
    , Liq(..)
    , mkLiq
    , Asset(..)
    , mkAsset
    , Account(..)
    , mkAccount
    , SubscribeRes(..)
    , AccountStateParams(..)
    , AccountStateRes(..)
    , mkAccountStateRes
    , CreatePoolParams(..)
    , CreatePoolRes(..)
    , AddFundsParams(..)
    , mkAddFundsParams
    , AddFundsRes(..)
    , RmFundsParams(..)
    , mkRmFundsParams
    , RmFundsRes(..)
    , AddLiqParams(..)
    , AddLiqRes(..)
    , mkAddLiqRes
    , RmLiqParams(..)
    , RmLiqRes(..)
    , mkRmLiqRes
    , SwapParams(..)
    , SwapRes(..)
    , mkSwapRes
    ) where

import Data.Aeson
import GHC.Generics

import Servant


data Pool = Pool
    { pLiq      :: Liq
    , pLPTokens :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON)

data Liq = Liq
    { lAssetA :: Asset
    , lAssetB :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkLiq :: Asset -> Asset -> Liq
mkLiq = Liq

data Asset = Asset
    { aName   :: String
    , aAmount :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAsset :: String -> Integer -> Asset
mkAsset = Asset

data Account = Account
    { aUserID :: String
    , aAssets :: [Asset]
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAccount :: String -> [Asset] -> Account
mkAccount = Account

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
type CreatePoolRes = Integer

newtype AddFundsParams = AddFundsParams { afpFunds :: [Asset] }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkAddFundsParams :: [Asset] -> AddFundsParams
mkAddFundsParams = AddFundsParams

newtype AddFundsRes = AddFundsRes { afrAccount :: Account }
  deriving (Eq, Show, Generic, ToJSON)

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
    { rlpLPTokens :: Integer
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
    , srAccount :: Account
    , srAsset   :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON)

mkSwapRes :: Pool -> Account -> Asset -> SwapRes
mkSwapRes = SwapRes
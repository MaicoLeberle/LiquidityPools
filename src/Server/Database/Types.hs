{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Types
    ( PoolRow
    , AccountRow
        -- Create pool.
    , CreatePoolParams(..)
        -- Retrieve account state.
    , GetAccountParams(..)
        -- Add funds to an account.
    , AddFundsParams(..)
        -- Remove funds from an account.
    , RmFundsParams(..)
        -- Add liquidity from an account to a pool.
    , AddLiqParams(..)
    , createPoolParamsToAddLiq
    , Transaction
        -- Remove liquidity from a pool to an account.
    , RmLiqParams(..)
        -- Swap liquidity at a pool.
    , SwapParams(..)
    ) where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics

import Types



{-| Required for parsing queries returning pool information, including Currency
    values.
-}
type PoolRow = ( Integer   -- pool ID
               , Currency  -- name of asset A
               , Integer   -- amount of asset A
               , Currency  -- name of asset B
               , Integer   -- amount of asset B
               , Integer   -- total number of liquidity tokens
               )

type AccountRow = ( Currency -- Asset name
                  , Integer  -- Asset amount
                  , String   -- User who owns this asset.
                  )

{-| Type Transaction allows for a compact and maintainable implementation of
    error-prone, multi-query sessions with the database backend.
-}
type Transaction a = ExceptT String IO a

-- | account endpoint.
newtype GetAccountParams = GetAccountParams { gapID :: String }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | createPool endpoint.
data CreatePoolParams = CreatePoolParams
    { cppPassword :: Password
    , cppLiq :: Liq
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | addFunds endpoint.
data AddFundsParams = AddFundsParams
    { afpPassword :: Password
    , afpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | rmFunds endpoint.
data RmFundsParams = RmFundsParams
    { rfpPassword :: Password
    , rfpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | addLiquidity endpoint.
data AddLiqParams = AddLiqParams
    { alpPassword :: Password
    , alpLiq      :: Liq
    }
  deriving (Eq, Show, Generic, FromJSON)

-- | Note that CreatePoolParams are isomorphic to AddLiqParams
createPoolParamsToAddLiq :: CreatePoolParams -> AddLiqParams
createPoolParamsToAddLiq cpp = AddLiqParams { alpPassword = cppPassword cpp
                                            , alpLiq = cppLiq cpp
                                            }

-- | rmLiquidity endpoint.
data RmLiqParams = RmLiqParams
    { rlpPass   :: Password
    , rlpPoolID :: Integer
    , rlpTokens :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | swap endpoint.
data SwapParams = SwapParams
    { spPassword :: Password
    , spAsset    :: Asset
    , spPoolID   :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

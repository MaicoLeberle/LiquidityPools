{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.Database
    (   -- | Used to compose/sequence actions on the database.
      Transaction
       {-| Retrieve list of pools, with utils to build a pool from a database
            query response.
        -}
    , GetPoolsRes
    , PoolRow
    , fromPoolRow
        {-| Get account state, with utils to build an account from a database
            query response.
        -}
    , GetAccountParams(..)
    , GetAccountRes
    , AccountRow
    , fromAccountRows
        -- | Create pool, with utils to add the initial liquidity.
    , CreatePoolParams(..)
    , CreatePoolRes
    , createPoolParamsToAddLiq
        -- | Create new user and retrieve password.
    , SubscribeRes
        -- | Add funds to an account.
    , AddFundsParams(..)
    , AddFundsRes
        -- | Remove funds from an account.
    , RmFundsParams(..)
    , RmFundsRes
        -- | Add liquidity from an account to a pool.
    , AddLiqParams(..)
    , AddLiqRes
        -- | Remove liquidity from a pool to an account.
    , RmLiqParams(..)
    , RmLiqRes
        -- Swap liquidity at a pool.
    , SwapParams(..)
    , SwapRes
    ) where

import Control.Monad.Trans.Except
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
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

import Types.Base


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



{-| Type Transaction allows for a compact and maintainable implementation of
    error-prone, multi-query sessions with the database backend.
-}
type Transaction a = ExceptT String IO a

-- | account endpoint types.
type GetPoolsRes = Either String [Pool]

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

fromPoolRow :: PoolRow -> Pool
fromPoolRow (pID, aAsset, aAmount, bAsset, bAmount, tokens) =
    mkPool pID (mkLiq (mkAsset aAsset aAmount) (mkAsset bAsset bAmount)) tokens

newtype GetAccountParams = GetAccountParams { gapID :: String }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type GetAccountRes = Either String Account

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

-- | createPool endpoint.
data CreatePoolParams = CreatePoolParams
    { cppPassword :: Password
    , cppLiq :: Liq
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type CreatePoolRes = Either String Integer

type SubscribeRes = Either String Password

-- | addFunds endpoint.
data AddFundsParams = AddFundsParams
    { afpPassword :: Password
    , afpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type AddFundsRes = Either String ()

-- | rmFunds endpoint.
data RmFundsParams = RmFundsParams
    { rfpPassword :: Password
    , rfpAsset :: Asset
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type RmFundsRes = Either String ()

-- | addLiquidity endpoint.
data AddLiqParams = AddLiqParams
    { alpPassword :: Password
    , alpLiq      :: Liq
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type AddLiqRes = Either String Integer

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

type RmLiqRes = Either String Liq

-- | swap endpoint.
data SwapParams = SwapParams
    { spPassword :: Password
    , spAsset    :: Asset
    , spPoolID   :: Integer
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type SwapRes = Either String Asset

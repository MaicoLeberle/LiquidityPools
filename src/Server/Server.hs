{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Server (app) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant

import           Server.Business
import qualified Server.Database as DB
import           Types.Database
import           Types.API


app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server =      getPools
         :<|> subscribe
         :<|> getAccount
         :<|> addFunds
         :<|> rmFunds
         :<|> createPool
         :<|> addLiquidity
         :<|> rmLiquidity
         :<|> swap

  where
    getPools :: Handler GetPoolsRes
    getPools = liftIO DB.pools

    subscribe :: Handler SubscribeRes
    subscribe = liftIO DB.subscribe

    getAccount :: GetAccountParams -> Handler GetAccountRes
    getAccount = liftIO . DB.getAccount

      -- Add a single asset to the account.
    addFunds :: AddFundsParams -> Handler AddFundsRes
    addFunds = liftIO . DB.addFunds

      -- Remove a single asset from the account.
    rmFunds :: RmFundsParams -> Handler RmFundsRes
    rmFunds = liftIO . DB.rmFunds

      -- Returns number of new liquidity tokens.
    createPool :: CreatePoolParams -> Handler CreatePoolRes
    createPool = liftIO . DB.createPool

      -- Returns number of new liquidity tokens.
    addLiquidity :: AddLiqParams -> Handler AddLiqRes
    addLiquidity = liftIO . DB.addLiquidity

      -- Returns liquidity removed from pool.
    rmLiquidity :: RmLiqParams -> Handler RmLiqRes
    rmLiquidity = liftIO . DB.rmLiquidity

    swap :: SwapParams -> Handler SwapRes
    swap = liftIO . DB.swap

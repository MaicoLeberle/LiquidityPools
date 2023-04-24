{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.Server (app) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant

import           Server.Business
import qualified Server.Database.Main  as DB
import           Types.Database


app :: Application
app = serve (Proxy :: Proxy API) server

type API =
         "pools"        :> Get '[JSON] GetPoolsRes
    :<|> "subscribe"    :> Get '[JSON] SubscribeRes
    :<|> "account"      :> ReqBody '[JSON] GetAccountParams
                        :> Post '[JSON] GetAccountRes
    :<|> "addFunds"     :> ReqBody '[JSON] AddFundsParams
                        :> Post '[JSON] AddFundsRes
    :<|> "rmFunds"      :> ReqBody '[JSON] RmFundsParams
                        :> Post '[JSON] RmFundsRes
    :<|> "createPool"   :> ReqBody '[JSON] CreatePoolParams
                        :> Post '[JSON] CreatePoolRes
    :<|> "addLiquidity" :> ReqBody '[JSON] AddLiqParams
                        :> Post '[JSON] AddLiqRes
    :<|> "rmLiquidity"  :> ReqBody '[JSON] RmLiqParams
                        :> Post '[JSON] RmLiqRes
    :<|> "swap"         :> ReqBody '[JSON] SwapParams
                        :> Post '[JSON] SwapRes

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
    getPools = runOnDB DB.pools

    subscribe :: Handler SubscribeRes
    subscribe = runOnDB DB.subscribe

    getAccount :: GetAccountParams -> Handler GetAccountRes
    getAccount = runOnDB . DB.getAccount

      -- Add a single asset to the account.
    addFunds :: AddFundsParams -> Handler AddFundsRes
    addFunds = runOnDB . DB.addFunds

      -- Remove a single asset from the account.
    rmFunds :: RmFundsParams -> Handler RmFundsRes
    rmFunds = runOnDB . DB.rmFunds

      -- Returns number of new liquidity tokens.
    createPool :: CreatePoolParams -> Handler CreatePoolRes
    createPool = runOnDB . DB.createPool

      -- Returns number of new liquidity tokens.
    addLiquidity :: AddLiqParams -> Handler AddLiqRes
    addLiquidity = runOnDB . DB.addLiquidity

      -- Returns liquidity removed from pool.
    rmLiquidity :: RmLiqParams -> Handler RmLiqRes
    rmLiquidity = runOnDB . DB.rmLiquidity

    swap :: SwapParams -> Handler SwapRes
    swap = runOnDB . DB.swap


-- | Auxiliary values.
runOnDB :: forall a b. IO a -> Handler a
runOnDB dbAction = liftIO dbAction >>= return

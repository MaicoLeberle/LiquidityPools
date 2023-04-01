{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators   #-}

module Server ( app ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant

import qualified Database.Main as DB

import qualified Business as B
import           Types    ( Pool(..)
                          , Account(..)
                          , Password
                          , SubscribeRes
                          , GetAccountParams(..)
                          , GetAccountRes(..)
                          , CreatePoolParams(..)
                          , CreatePoolRes(..)
                          , AddFundsParams(..)
                          , AddFundsRes(..)
                          , RmFundsParams(..)
                          , RmFundsRes(..)
                          , AddLiqParams(..)
                          , AddLiqRes(..)
                          , RmLiqParams(..)
                          , RmLiqRes(..)
                          , SwapParams(..)
                          , SwapRes(..)
                          )


app :: Application
app = serve (Proxy :: Proxy API) server

type API =
         "pools"        :> Get '[JSON] [Pool]
    :<|> "subscribe"    :> Get '[JSON] SubscribeRes
    :<|> "account"      :> ReqBody '[JSON] GetAccountParams
                        :> Post '[JSON] GetAccountRes
    :<|> "createPool"   :> ReqBody '[JSON] CreatePoolParams
                        :> Post '[JSON] CreatePoolRes
    -- :<|> "addLiquidity" :> ReqBody '[JSON] AddLiqParams
    --                     :> Post '[JSON] (Maybe AddLiqRes)
    -- :<|> "rmLiquidity"  :> ReqBody '[JSON] RmLiqParams
    --                     :> Post '[JSON] (Maybe RmLiqRes)
    :<|> "addFunds"     :> ReqBody '[JSON] AddFundsParams
                        :> Post '[JSON] AddFundsRes
    :<|> "rmFunds"      :> ReqBody '[JSON] RmFundsParams
                        :> Post '[JSON] RmFundsRes
    -- :<|> "swap"         :> ReqBody '[JSON] SwapParams
    --                     :> Post '[JSON] (Maybe SwapRes)

server :: Server API
server =      listPools
         :<|> subscribe
         :<|> accountState
         :<|> createPool
         -- :<|> addLiquidity
         -- :<|> rmLiquidity
         :<|> addFunds
         :<|> rmFunds
         -- :<|> swap

  where
      -- GET requests.
    listPools :: Handler [Pool]
    listPools = runOnDB DB.pools

    subscribe :: Handler Password
    subscribe = runOnDB DB.createUser
      -- POST requests.
    accountState :: GetAccountParams -> Handler GetAccountRes
    accountState = runOnDB . DB.getAccount

    createPool :: CreatePoolParams -> Handler CreatePoolRes
    createPool = runOnDB . DB.createPool

    -- addLiquidity :: AddLiqParams -> Handler (Maybe AddLiqRes)
    -- addLiquidity = return . B.addLiq B.somePool

    -- rmLiquidity :: RmLiqParams -> Handler (Maybe RmLiqRes)
    -- rmLiquidity = return . B.rmLiq B.somePool

    addFunds :: AddFundsParams -> Handler AddFundsRes
    addFunds = runOnDB . DB.addFunds

    rmFunds :: RmFundsParams -> Handler RmFundsRes
    rmFunds = runOnDB . DB.rmFunds

    -- swap :: SwapParams -> Handler (Maybe SwapRes)
    -- swap = return . B.swap B.somePool


-- | Auxiliary values.
runOnDB :: forall a b. IO a -> Handler a
runOnDB dbAction = liftIO dbAction >>= return

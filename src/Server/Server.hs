{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Server ( app ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant

import qualified Business as B
import           Types    ( Pool(..)
                          , Account(..)
                          , SubscribeRes
                          , AccountStateParams(..)
                          , AccountStateRes(..)
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
         "createPool"   :> ReqBody '[JSON] CreatePoolParams
                        :> Post '[JSON] (Maybe CreatePoolRes)
    :<|> "pools"        :> Get '[JSON] [Pool]
    :<|> "subscribe"    :> Get '[JSON] SubscribeRes
    :<|> "account"      :> ReqBody '[JSON] AccountStateParams
                        :> Post '[JSON] (Maybe AccountStateRes)
    :<|> "addLiquidity" :> ReqBody '[JSON] AddLiqParams
                        :> Post '[JSON] (Maybe AddLiqRes)
    :<|> "rmLiquidity"  :> ReqBody '[JSON] RmLiqParams
                        :> Post '[JSON] (Maybe RmLiqRes)
    :<|> "addFunds"     :> ReqBody '[JSON] AddFundsParams
                        :> Post '[JSON] (Maybe AddFundsRes)
    :<|> "rmFunds"      :> ReqBody '[JSON] RmFundsParams
                        :> Post '[JSON] (Maybe RmFundsRes)
    :<|> "swap"         :> ReqBody '[JSON] SwapParams
                        :> Post '[JSON] (Maybe SwapRes)

server :: Server API
server =      createPool
         :<|> listPools
         :<|> subscribe
         :<|> accountState
         :<|> addLiquidity
         :<|> rmLiquidity
         :<|> addFunds
         :<|> rmFunds
         :<|> swap

  where
    listPools :: Handler [Pool]
    listPools = return B.listPools

    subscribe :: Handler String
    subscribe = liftIO B.createUserID >>= return

    accountState :: AccountStateParams -> Handler (Maybe AccountStateRes)
    accountState = return . B.accountState B.someAccounts

    createPool :: CreatePoolParams -> Handler (Maybe CreatePoolRes)
    createPool = return . B.createPool

    addLiquidity :: AddLiqParams -> Handler (Maybe AddLiqRes)
    addLiquidity = return . B.addLiq B.somePool

    rmLiquidity :: RmLiqParams -> Handler (Maybe RmLiqRes)
    rmLiquidity = return . B.rmLiq B.somePool

    addFunds :: AddFundsParams -> Handler (Maybe AddFundsRes)
    addFunds = return . B.addFunds B.someAccount

    rmFunds :: RmFundsParams -> Handler (Maybe RmFundsRes)
    rmFunds = return . B.rmFunds B.someAccount

    swap :: SwapParams -> Handler (Maybe SwapRes)
    swap = return . B.swap B.somePool

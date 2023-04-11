{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server ( app ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant

import           Business
import qualified Database.Main  as DB
import           Database.Types
import           Types


app :: Application
app = serve (Proxy :: Proxy API) server

type API =
         "pools"        :> Get '[JSON] GetPoolsRes
    :<|> "subscribe"    :> Get '[JSON] SubscribeRes
    :<|> "account"      :> ReqBody '[JSON] GetAccountParams
                        :> Post '[JSON] GetAccountRes
    :<|> "createPool"   :> ReqBody '[JSON] CreatePoolParams
                        :> Post '[JSON] CreatePoolRes
    :<|> "addLiquidity" :> ReqBody '[JSON] AddLiqParams
                        :> Post '[JSON] AddLiqRes
    :<|> "rmLiquidity"  :> ReqBody '[JSON] RmLiqParams
                        :> Post '[JSON] RmLiqRes
    :<|> "addFunds"     :> ReqBody '[JSON] AddFundsParams
                        :> Post '[JSON] AddFundsRes
    :<|> "rmFunds"      :> ReqBody '[JSON] RmFundsParams
                        :> Post '[JSON] RmFundsRes
    :<|> "swap"         :> ReqBody '[JSON] SwapParams
                        :> Post '[JSON] SwapRes

server :: Server API
server =      listPools
         :<|> subscribe
         :<|> accountState
         :<|> createPool
         :<|> addLiquidity
         :<|> rmLiquidity
         :<|> addFunds
         :<|> rmFunds
         :<|> swap

  where
      -- GET requests.
    listPools :: Handler GetPoolsRes
    listPools = runOnDB DB.pools

    subscribe :: Handler SubscribeRes
    subscribe = runOnDB DB.createUser

      -- POST requests.
    accountState :: GetAccountParams -> Handler GetAccountRes
    accountState = runOnDB . DB.getAccount

      -- Returns number of new liquidity tokens.
    createPool :: CreatePoolParams -> Handler CreatePoolRes
    createPool = runOnDB . DB.createPool

      -- Returns number of new liquidity tokens.
    addLiquidity :: AddLiqParams -> Handler AddLiqRes
    addLiquidity = runOnDB . DB.addLiquidity

      -- Returns liquidity removed from pool.
    rmLiquidity :: RmLiqParams -> Handler RmLiqRes
    rmLiquidity = runOnDB . DB.rmLiquidity

    addFunds :: AddFundsParams -> Handler AddFundsRes
    addFunds = runOnDB . DB.addFunds

    rmFunds :: RmFundsParams -> Handler RmFundsRes
    rmFunds = runOnDB . DB.rmFunds

    swap :: SwapParams -> Handler SwapRes
    swap = runOnDB . DB.swap


-- | Auxiliary values.
runOnDB :: forall a b. IO a -> Handler a
runOnDB dbAction = liftIO dbAction >>= return

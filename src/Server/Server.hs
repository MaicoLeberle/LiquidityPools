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

import           Business
import qualified Database.Main  as DB
import           Database.Types
import           Types


app :: Application
app = serve (Proxy :: Proxy API) server

type API =
         "pools"        :> Get '[JSON] (Either String [Pool])
    :<|> "subscribe"    :> Get '[JSON] (Either String String)
    :<|> "account"      :> ReqBody '[JSON] GetAccountParams
                        :> Post '[JSON] (Either String Account)
    :<|> "createPool"   :> ReqBody '[JSON] CreatePoolParams
                        :> Post '[JSON] (Either String Integer)
    :<|> "addLiquidity" :> ReqBody '[JSON] AddLiqParams
                        :> Post '[JSON] (Either String Integer)
    :<|> "rmLiquidity"  :> ReqBody '[JSON] RmLiqParams
                        :> Post '[JSON] (Either String Liq)
    :<|> "addFunds"     :> ReqBody '[JSON] AddFundsParams
                        :> Post '[JSON] (Either String ())
    :<|> "rmFunds"      :> ReqBody '[JSON] RmFundsParams
                        :> Post '[JSON] (Either String ())
    :<|> "swap"         :> ReqBody '[JSON] SwapParams
                        :> Post '[JSON] (Either String Asset)

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
    listPools :: Handler (Either String [Pool])
    listPools = runOnDB DB.pools

    subscribe :: Handler (Either String Password)
    subscribe = runOnDB DB.createUser

      -- POST requests.
    accountState :: GetAccountParams -> Handler (Either String Account)
    accountState = runOnDB . DB.getAccount

      -- Returns number of new liquidity tokens.
    createPool :: CreatePoolParams -> Handler (Either String Integer)
    createPool = runOnDB . DB.createPool

      -- Returns number of new liquidity tokens.
    addLiquidity :: AddLiqParams -> Handler (Either String Integer)
    addLiquidity = runOnDB . DB.addLiquidity

      -- Returns liquidity removed from pool.
    rmLiquidity :: RmLiqParams -> Handler (Either String Liq)
    rmLiquidity = runOnDB . DB.rmLiquidity

    addFunds :: AddFundsParams -> Handler (Either String ())
    addFunds = runOnDB . DB.addFunds

    rmFunds :: RmFundsParams -> Handler (Either String ())
    rmFunds = runOnDB . DB.rmFunds

    swap :: SwapParams -> Handler (Either String Asset)
    swap = runOnDB . DB.swap


-- | Auxiliary values.
runOnDB :: forall a b. IO a -> Handler a
runOnDB dbAction = liftIO dbAction >>= return

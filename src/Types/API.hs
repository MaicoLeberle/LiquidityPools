{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Types.API
    ( API
    , ServerResponse
    ) where

import Data.Aeson

import Servant.API

import Types.Base
import Types.Database


type API =      "pools"        :> Get '[JSON] GetPoolsRes
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

data ServerResponse where
  GetPoolsSR :: Either String GetPoolsRes -> ServerResponse
  SubscribeSR :: Either String SubscribeRes -> ServerResponse
  GetAccountSR :: Either String GetAccountRes -> ServerResponse
  AddFundsSR :: Either String AddFundsRes -> ServerResponse
  RmFundsSR :: Either String RmFundsRes -> ServerResponse
  CreatePoolSR :: Either String CreatePoolRes -> ServerResponse
  AddLiqSR :: Either String AddLiqRes -> ServerResponse
  RmLiqSR :: Either String RmLiqRes -> ServerResponse
  SwapSR :: Either String SwapRes -> ServerResponse

instance Show ServerResponse where
  show (GetPoolsSR (Left res))    = res
  show (GetPoolsSR (Right res))   = show res
  show (SubscribeSR (Left res))   = res
  show (SubscribeSR (Right res))  = show res
  show (GetAccountSR (Left res))  = res
  show (GetAccountSR (Right res)) = show res
  show (AddFundsSR (Left res))    = res
  show (AddFundsSR (Right res))   = show res
  show (RmFundsSR (Left res))     = res
  show (RmFundsSR (Right res))    = show res
  show (CreatePoolSR (Left res))  = res
  show (CreatePoolSR (Right res)) = show res
  show (AddLiqSR (Left res))      = res
  show (AddLiqSR (Right res))     = show res
  show (RmLiqSR (Left res))       = res
  show (RmLiqSR (Right res))      = show res
  show (SwapSR (Left res))        = res
  show (SwapSR (Right res))       = show res
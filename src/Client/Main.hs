{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Types.Database


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

myAPI :: Proxy API
myAPI = Proxy

(getPools :<|> subscribe
          :<|> getAccount
          :<|> addFunds
          :<|> rmFunds
          :<|> createPool
          :<|> addLiquidity
          :<|> rmLiquidity
          :<|> swap) = client myAPI


main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  pools <- runClientM getPools (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case pools of
    Left err -> putStrLn $ "Error: " ++ show err
    Right res -> print res
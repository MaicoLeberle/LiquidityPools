module Main where

import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Types.Database
import Types.API

myAPI :: Proxy API
myAPI = Proxy

(     getPools
 :<|> subscribe
 :<|> getAccount
 :<|> addFunds
 :<|> rmFunds
 :<|> createPool
 :<|> addLiquidity
 :<|> rmLiquidity
 :<|> swap         ) = client myAPI

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  pools <- runClientM getPools (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case pools of
    Left err -> putStrLn $ "Error: " ++ show err
    Right res -> print res
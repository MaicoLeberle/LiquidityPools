{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Proxy
import Data.String (fromString)
import Network.HTTP.Client      (newManager, defaultManagerSettings, Manager(..))
import Language.Haskell.TH
import Servant.API
import Servant.Client
import System.Console.Haskeline
import Text.Read (readMaybe)

import Types.Base
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
  manager <- newManager defaultManagerSettings
  pools <- runClientM getPools
                      (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case pools of
    Left err -> putStrLn $ "Error: " ++ show err
    Right res -> print res


main' :: IO ()
main' = do
    putStrLn "A CLI for the liquidity pools client."
    putStrLn "(\":q\" or CTRL+C to quit)\n"
    manager <- newManager defaultManagerSettings
    runInputT defaultSettings
              (loop (mkClientEnv manager (BaseUrl Http "localhost" 8081 "")))

loop :: ClientEnv -> InputT IO ()
loop env = printMenu >> execAction env

printMenu :: InputT IO ()
printMenu = outputStrLn $ concat  [ "1. List active pools."
                                  , "2. Subscribe new account."
                                  , "3. Retrieve account state."
                                  , "4. Add funds to an existing account."
                                  , "5. Remove funds from an existing account."
                                  , "6. Create pool."
                                  , "7. Add liquidity."
                                  , "8. Remove liquidity."
                                  , "9. Swap assets."
                                  ]

execAction :: ClientEnv -> InputT IO ()
execAction env = do maybeOption <- getInputLine "Option: "
                    case maybeOption of
                        Nothing  -> execAction env
                        Just res | res == "1"   -> getPools' env
                                 | res == "2"   -> subscribe' env
                                 | res == "3"   -> getAccount' env
                                 | res == "4"   -> addFunds' env
                                 | res == "5"   -> rmFunds' env
                                 | res == "6"   -> createPool' env
                                 | res == "7"   -> addLiquidity' env
                                 | res == "8"   -> rmLiquidity' env
                                 | res == "9"   -> swap' env
                                 | res == ":q"  -> return ()

getPools' :: ClientEnv -> InputT IO ()
getPools' env = lift (runClientM getPools env) >>= printRes >> loop env

subscribe' :: ClientEnv -> InputT IO ()
subscribe' env = lift (runClientM subscribe env) >>= printRes >> loop env

getAccount' :: ClientEnv -> InputT IO ()
getAccount' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
           Nothing -> getAccount' env
           Just pass ->
                do res <- lift (runClientM (getAccount $ fromString pass) env)
                   printRes res
                   loop env

addFunds' :: ClientEnv -> InputT IO ()
addFunds' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
            Nothing -> addFunds' env
            Just pass -> getCurr $ AddFundsParams pass undefined
  where
    getCurr :: AddFundsParams -> InputT IO ()
    getCurr afp =
        do maybeCurr <- getInputLine "Currency kind (ARS, EUR, GBP, USD): "
           case maybeCurr >>= readMaybe @Currency of
                Nothing   -> getCurr afp
                Just curr -> getAmount afp { afpAsset = mkAsset curr undefined}

    getAmount :: AddFundsParams -> InputT IO ()
    getAmount afp@AddFundsParams{..} =
        do maybeAmount <- getInputLine "Amount: "
           case maybeAmount >>= readMaybe @Integer of
                Nothing -> getAmount afp
                Just amount ->
                    do let finalAfp =
                            afp { afpAsset = afpAsset {aAmount = amount}}
                       res <- lift $ runClientM (addFunds finalAfp) env
                       printRes res
                       loop env

rmFunds' :: ClientEnv -> InputT IO ()
rmFunds' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
            Nothing -> rmFunds' env
            Just pass -> getCurr $ RmFundsParams pass undefined
  where
    getCurr :: RmFundsParams -> InputT IO ()
    getCurr rfp =
        do maybeCurr <- getInputLine "Currency kind (ARS, EUR, GBP, USD): "
           case maybeCurr >>= readMaybe @Currency of
                Nothing   -> getCurr rfp
                Just curr -> getAmount rfp { rfpAsset = mkAsset curr undefined}

    getAmount :: RmFundsParams -> InputT IO ()
    getAmount rfp@RmFundsParams{..} =
        do maybeAmount <- getInputLine "Amount: "
           case maybeAmount >>= readMaybe @Integer of
                Nothing -> getAmount rfp
                Just amount ->
                    do let finalRfp =
                            rfp { rfpAsset = rfpAsset {aAmount = amount}}
                       res <- lift $ runClientM (rmFunds finalRfp) env
                       printRes res
                       loop env

createPool' :: ClientEnv -> InputT IO ()
createPool' = addLiqAux createPool mkCreatePoolParams

addLiquidity' :: ClientEnv -> InputT IO ()
addLiquidity' = addLiqAux addLiquidity mkAddLiqParams

rmLiquidity' :: ClientEnv -> InputT IO ()
rmLiquidity' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
            Nothing -> rmLiquidity' env
            Just pass -> getPoolID $ RmLiqParams pass undefined undefined
  where
    getPoolID :: RmLiqParams -> InputT IO ()
    getPoolID rlp =
        do maybeCurr <- getInputLine "Pool ID: "
           case maybeCurr >>= readMaybe @Integer of
                Nothing   -> getPoolID rlp
                Just id -> getTokens rlp {rlpPoolID = id}

    getTokens :: RmLiqParams -> InputT IO ()
    getTokens rlp =
        do maybeAmount <- getInputLine "Number of tokens to exchange: "
           case maybeAmount >>= readMaybe @Integer of
                Nothing -> getTokens rlp
                Just amount ->
                    do let finalrlp = rlp {rlpTokens = amount}
                       res <- lift $ runClientM
                                        (rmLiquidity rlp {rlpTokens = amount})
                                        env
                       printRes res
                       loop env

swap' :: ClientEnv -> InputT IO ()
swap' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
            Nothing -> createPool' env
            Just pass -> getPoolID $ SwapParams pass undefined undefined
  where
    getPoolID :: SwapParams -> InputT IO ()
    getPoolID sp =
        do maybeCurr <- getInputLine "Pool ID: "
           case maybeCurr >>= readMaybe @Integer of
                Nothing -> getPoolID sp
                Just id -> getCurr sp {spPoolID = id}

    getCurr :: SwapParams -> InputT IO ()
    getCurr sp@SwapParams{..} =
        do maybeAmount <- getInputLine "Currency kind (ARS, EUR, GBP, USD): "
           case maybeAmount >>= readMaybe @Currency of
                Nothing   -> getCurr sp
                Just curr -> getAmount sp {spAsset = spAsset {aName = curr}}

    getAmount :: SwapParams -> InputT IO ()
    getAmount sp@SwapParams{..} =
        do maybeCurr <- getInputLine "Amount: "
           case maybeCurr >>= readMaybe @Integer of
                Nothing     -> getAmount sp
                Just amount ->
                    do let finalSp = sp {spAsset = spAsset {aAmount = amount}}
                       res <- lift $ runClientM (swap finalSp) env
                       printRes res
                       loop env


-- | Auxiliary utils.
{-| addLiqAux is used both for the createPool and addLiquidity endpoints, whose
    respective parameter types, namely CreatePoolParams and AddLiqParams, are
    isomorphic, hence the code for the user to input them should be shared.
-}
addLiqAux :: Show b
          => (a -> ClientM (Either String b))
          -> (Password -> Liq -> a)
          -> ClientEnv
          -> InputT IO ()
addLiqAux func pCons env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
            Nothing -> createPool' env
            Just pass -> getFirstCurr $ (pass, undefined)
  where
    getFirstCurr :: (Password, Liq) -> InputT IO ()
    getFirstCurr (pass, liq) =
        do maybeCurr <-
                getInputLine "First currency kind (ARS, EUR, GBP, USD): "
           case maybeCurr >>= readMaybe @Currency of
                Nothing   -> getFirstCurr (pass, liq)
                Just curr -> getFirstAmount ( pass
                                            , mkLiq (mkAsset curr undefined)
                                                    undefined
                                            )

    getFirstAmount :: (Password, Liq) -> InputT IO ()
    getFirstAmount (pass, liq) =
        do maybeAmount <- getInputLine "Amount: "
           case maybeAmount >>= readMaybe @Integer of
                Nothing -> getFirstAmount (pass, liq)
                Just amount ->
                    getSecCurr ( pass
                               , liq {lAssetA =
                                        (lAssetA liq) {aAmount = amount}}
                               )

    getSecCurr :: (Password, Liq) -> InputT IO ()
    getSecCurr (pass, liq) =
        do maybeCurr <-
            getInputLine "Second currency kind (ARS, EUR, GBP, USD): "
           case maybeCurr >>= readMaybe @Currency of
                Nothing   -> getSecCurr (pass, liq)
                Just curr ->
                    getSecAmount ( pass
                                 , liq {lAssetB = (lAssetB liq) {aName = curr}}
                                 )

    getSecAmount :: (Password, Liq) -> InputT IO ()
    getSecAmount (pass, liq) =
        do maybeAmount <- getInputLine "Amount: "
           case maybeAmount >>= readMaybe @Integer of
                Nothing -> getFirstAmount (pass, liq)
                Just amount -> do
                    let finalLiq =
                            liq {lAssetB = (lAssetB liq) {aAmount = amount}}
                    res <- lift $ runClientM (func $ pCons pass finalLiq) env
                    printRes res
                    loop env

printRes :: Show a => Either ClientError (Either String a) -> InputT IO ()
printRes (Left         clientErr) = outputStrLn $ show clientErr
printRes (Right (Left customErr)) = outputStrLn customErr
printRes (Right (Right      res)) = outputStrLn $ show res

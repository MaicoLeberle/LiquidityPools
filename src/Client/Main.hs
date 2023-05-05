{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Proxy
import Data.String               (fromString)
import Network.HTTP.Client       ( newManager
                                 , defaultManagerSettings
                                 , Manager(..)
                                 )
import Servant.API
import Servant.Client
import System.Console.Haskeline
import Text.Read                 (readMaybe)

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
    putStrLn "A CLI for the liquidity pools client."
    putStrLn "(:q or CTRL+C to quit)\n"
    manager <- newManager defaultManagerSettings
    runInputT defaultSettings
              (loop (mkClientEnv manager (BaseUrl Http "localhost" 8081 "")))

loop :: ClientEnv -> InputT IO ()
loop env = printMenu >> execAction env

printMenu :: InputT IO ()
printMenu = outputStrLn $ concat [ "1. List active pools.\n"
                                 , "2. Subscribe new account.\n"
                                 , "3. Retrieve account state.\n"
                                 , "4. Add funds to an existing account.\n"
                                 , "5. Remove funds from an existing account.\n"
                                 , "6. Create pool.\n"
                                 , "7. Add liquidity.\n"
                                 , "8. Remove liquidity.\n"
                                 , "9. Swap assets.\n"
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
                                 | otherwise    -> execAction env

getPools' :: ClientEnv -> InputT IO ()
getPools' = getRequests getPools

subscribe' :: ClientEnv -> InputT IO ()
subscribe' = getRequests subscribe

getAccount' :: ClientEnv -> InputT IO ()
getAccount' env =
    do maybePass <- getInputLine "Password: "
       case maybePass of
           Nothing -> getAccount' env
           Just pass -> callEndpoint env getAccount (fromString pass)

addFunds' :: ClientEnv -> InputT IO ()
addFunds' env =
        updParams @AddFundsParams @Password "Password: " insPass initAddFundsP
    >>= updParams @AddFundsParams @Currency
            ("Currency kind " ++ currencyKinds ++ ": ") insCurr
    >>= updParams @AddFundsParams @Integer "Amount: " insAmount
    >>= callEndpoint env addFunds
  where
    initAddFundsP :: AddFundsParams
    initAddFundsP = mkAddFundsParams undefined undefined

    insPass :: AddFundsParams -> Password -> AddFundsParams
    insPass afp pass = afp {afpPassword = pass}

    insCurr :: AddFundsParams -> Currency -> AddFundsParams
    insCurr afp c = afp {afpAsset = mkAsset c undefined}

    insAmount :: AddFundsParams -> Integer -> AddFundsParams
    insAmount afp n = afp {afpAsset = (afpAsset afp) {aAmount = n}}

rmFunds' :: ClientEnv -> InputT IO ()
rmFunds' env =
        updParams @RmFundsParams @Password "Password: " insPass initRmFundsP
    >>= updParams @RmFundsParams @Currency
            ("Currency kind " ++ currencyKinds ++ ": ") insCurr
    >>= updParams @RmFundsParams @Integer "Amount: " insAmount
    >>= callEndpoint env rmFunds
  where
    initRmFundsP :: RmFundsParams
    initRmFundsP = mkRmFundsParams undefined undefined

    insPass :: RmFundsParams -> Password -> RmFundsParams
    insPass rfp pass = rfp {rfpPassword = pass}

    insCurr :: RmFundsParams -> Currency -> RmFundsParams
    insCurr rfp c = rfp {rfpAsset = mkAsset c undefined}

    insAmount  :: RmFundsParams -> Integer -> RmFundsParams
    insAmount rfp n = rfp {rfpAsset = (rfpAsset rfp) {aAmount = n}}

createPool' :: ClientEnv -> InputT IO ()
createPool' = addLiqAux createPool mkCreatePoolParams

addLiquidity' :: ClientEnv -> InputT IO ()
addLiquidity' = addLiqAux addLiquidity mkAddLiqParams

rmLiquidity' :: ClientEnv -> InputT IO ()
rmLiquidity' env =
        updParams @RmLiqParams @Password "Password: " insPass initRmLiqP
    >>= updParams @RmLiqParams @Integer "Pool ID: " insPoolID
    >>= updParams @RmLiqParams @Integer
            "Number of tokens to exchange: " insTokens
    >>= callEndpoint env rmLiquidity
  where
    initRmLiqP :: RmLiqParams
    initRmLiqP = mkRmLiqParams undefined undefined undefined

    insPass :: RmLiqParams -> Password -> RmLiqParams
    insPass rlp pass = rlp {rlpPassword = pass}

    insPoolID :: RmLiqParams -> Integer -> RmLiqParams
    insPoolID rlp id = rlp {rlpPoolID = id}

    insTokens :: RmLiqParams -> Integer -> RmLiqParams
    insTokens rlp n = rlp {rlpTokens = n}

swap' :: ClientEnv -> InputT IO ()
swap' env =
        updParams @SwapParams @Password "Password: " insPass initSwapP
    >>= updParams @SwapParams @Integer "Pool ID: " insPoolID
    >>= updParams @SwapParams @Currency ("Currency " ++ currencyKinds ++ ": ")
                                        insCurr
    >>= updParams @SwapParams @Integer "Amount: " insAmount
    >>= callEndpoint env swap
  where
    initSwapP :: SwapParams
    initSwapP = SwapParams undefined undefined undefined

    insPass :: SwapParams -> Password -> SwapParams
    insPass sp pass = sp {spPassword = pass}

    insPoolID :: SwapParams -> Integer -> SwapParams
    insPoolID sp id = sp {spPoolID = id}

    insCurr :: SwapParams -> Currency -> SwapParams
    insCurr sp c = sp {spAsset = mkAsset c undefined}

    insAmount :: SwapParams -> Integer -> SwapParams
    insAmount sp am = sp {spAsset = (spAsset sp) {aAmount = am}}

-- | Auxiliary utils.
-- | getRequests generalizes GET HTTP requests, namely getPools and subscribe.
getRequests :: Show a => ClientM (Either String a) -> ClientEnv -> InputT IO ()
getRequests action env = lift (runClientM action env) >>= printRes >> loop env

{-| addLiqAux is used both for the createPool and addLiquidity endpoints, whose
    respective parameter types, namely CreatePoolParams and AddLiqParams, are
    isomorphic, hence the code for the user to input them should be shared.
-}
addLiqAux :: forall a b
          . Show b
          => (a -> ClientM (Either String b))
          -> (Password -> Liq -> a)
          -> ClientEnv
          -> InputT IO ()
addLiqAux func paramsCons env =
        updParams @(Password, Liq) @Password "Password: " insPass initParams
    >>= updParams @(Password, Liq) @Currency
            ("First currency " ++ currencyKinds ++ ": ") insFstCurr
    >>= updParams @(Password, Liq) @Integer "Amount: " insFstAmount
    >>= updParams @(Password, Liq) @Currency
            ("Second currency " ++ currencyKinds ++ ": ") insSndCurr
    >>= updParams @(Password, Liq) @Integer "Amount: " insSndAmount
    >>= return . uncurry paramsCons
    >>= callEndpoint env func
  where
    initParams :: (Password, Liq)
    initParams = (undefined, undefined)

    insPass :: (Password, Liq) -> Password -> (Password, Liq)
    insPass (_, liq) pass = (pass, liq)

    insFstCurr :: (Password, Liq) -> Currency -> (Password, Liq)
    insFstCurr (pass, liq) fc =
        (pass, liq {lAssetA = mkAsset fc undefined})

    insFstAmount :: (Password, Liq) -> Integer -> (Password, Liq)
    insFstAmount (pass, liq) fa =
        (pass, liq {lAssetA = (lAssetA liq) {aAmount = fa}})

    insSndCurr :: (Password, Liq) -> Currency -> (Password, Liq)
    insSndCurr (pass, liq) sc =
        (pass, liq {lAssetA = mkAsset sc undefined})

    insSndAmount :: (Password, Liq) -> Integer -> (Password, Liq)
    insSndAmount (pass, liq) sa =
        (pass, liq {lAssetB = (lAssetB liq) {aAmount = sa}})

printRes :: Show a => Either ClientError (Either String a) -> InputT IO ()
printRes (Left clientErr) =
    outputStrLn $ "\n" ++  show clientErr ++ "\n"
printRes (Right (Left customErr)) =
    outputStrLn $ "\n" ++  customErr ++ "\n"
printRes (Right (Right res)) =
    outputStrLn $ "\n" ++  show res ++ "\n"

currencyKinds :: String
currencyKinds = "(" ++ concatCurrKinds $(fieldNames ''Currency) ++ ")"
  where
    concatCurrKinds :: [String] -> String
    concatCurrKinds     [] = ""
    concatCurrKinds    [c] = c
    concatCurrKinds (c:cc) = c ++ ", " ++ concatCurrKinds cc

updParams :: forall a b
          . Read b
          => String
          -> (a -> b -> a)
          -> a
          -> InputT IO a
updParams msg updAcc acc =
    do maybeNewVal <- getInputLine msg
       case maybeNewVal >>= readMaybe @b of
            Nothing -> updParams msg updAcc acc
            Just newVal -> return $ updAcc acc newVal

callEndpoint :: forall a b
             . Show b
             => ClientEnv
             -> (a -> ClientM (Either String b))
             -> a
             -> InputT IO ()
callEndpoint env endpoint param =
    do res <- lift $ runClientM (endpoint param) env
       printRes res
       loop env

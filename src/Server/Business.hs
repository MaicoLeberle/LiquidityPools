{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Business
    (   -- Mock values.
      someAccount
    , someAccounts
    , somePool
    , somePools
        -- Actions.
    -- , listPools
    -- , addFunds
    -- , addLiq
    , initialTokens
    , createUserID
    -- , newUserID
    -- , rmFunds
    -- , rmLiq
    -- , swap
    ) where

import           Data.List
import qualified Data.Tuple as T
import           System.Random

import Types


somePool :: Pool
somePool =
    mkPool 1 (mkLiq (mkAsset ARS 3_900_000) (mkAsset USD 10_000)) 197_484

somePools :: [Pool]
somePools =
    [ somePool
    , mkPool 2 (mkLiq (mkAsset ARS 3_760_000) (mkAsset EUR 10_000)) 193_907
    ]

someAccount :: Account
someAccount = mkAccount "IQ7AXV039YG60HGMUKG9SIXC67JE4U"
                        [ mkAsset ARS 300_000
                        , mkAsset USD 50_000
                        , mkAsset EUR 10_000
                        ]

someAccounts :: [Account]
someAccounts = [ someAccount
               , mkAccount "DDF89NGNFVHUFQXGB8YP68GVGWA5CR"
                           [ mkAsset ARS 30_000
                           , mkAsset USD 500_000
                           , mkAsset EUR 76_000
                           ]
               ]

-- GET requests.
createUserID :: IO Password
createUserID = newUserID 30

newUserID :: Int -> IO String
newUserID n = map (vals !!) <$> (randomChar n 0 $ length vals - 1)
  where
    vals :: String
    vals = ['0'..'9'] ++ ['A'..'Z']

    randomChar :: Int -> Int -> Int -> IO [Int]
    randomChar n a b = sequence $ take n $ repeat $ randomRIO (a, b)

-- listPools :: [Pool]
-- listPools = somePools

-- -- POST requests.
{-  The number of new LP tokens is given by the underlying theory--c.f.
    "Formal Specification of Constant Product (x * y = k) Market Maker Model and
    Implementation": <https://github.com/runtimeverification/verified-smart-contracts/blob/c40c98d6ae35148b76742aaaa29e6eaa405b2f93/uniswap/x-y-k.pdf>
-}
initialTokens :: CreatePoolParams -> Integer
initialTokens CreatePoolParams{cppLiq=Liq{lAssetA=a, lAssetB=b},..} =
   floor $ sqrt $ fromInteger $ aAmount a * aAmount b

-- addFunds :: Account -> AddFundsParams -> Maybe AddFundsRes
-- addFunds a@Account{..} AddFundsParams{..} = addFundsAux afpFunds []
--   where
--     addFundsAux :: [Asset] -> [Asset] -> Maybe AddFundsRes
--     addFundsAux [] ff' = Just $ mkAddFundsRes $ a { aAssets = ff'}
--     addFundsAux (f : ff) ff' | aAmount f <= 0 = Nothing
--                              |      otherwise =
--         case find ((==) (aName f) . aName) aAssets of
--             Just f' -> addFundsAux ff $ mkAsset (aName f)
--                                                 (aAmount f' + aAmount f) : ff'
--             Nothing -> addFundsAux ff $ mkAsset (aName f) (aAmount f) : ff'

-- rmFunds :: Account -> RmFundsParams -> Maybe RmFundsRes
-- rmFunds a@Account{..} RmFundsParams{..} = rmFundsAux rfpFunds []
--   where
--     rmFundsAux :: [Asset] -> [Asset] -> Maybe RmFundsRes
--     rmFundsAux [] res = Just $ RmFundsRes $ a {aAssets = res}
--     rmFundsAux (f : ff) ff' | aAmount f <= 0 = Nothing
--                             |      otherwise =
--         case find ((==) (aName f) . aName) aAssets of
--             Just f' | aAmount f' < aAmount f -> Nothing
--                     |              otherwise ->
--                         rmFundsAux ff $ mkAsset (aName f)
--                                                 (aAmount f' - aAmount f ) : ff'
--             Nothing -> Nothing

-- {-  FIX: Implement preservation of the constant factor invariant and compute the
--     right number of LP tokens.
--     In particular, note that we assume here that asset `aAssetA` will be added
--     entirely to the liquidity of the pool, while only an appropriate amount of
--     asset aAssetB will be added in order to preserve the constant factor
--     invariant. In some cases, however, this may not be correct direction of
--     restrictions, as we might need to proceed in the inverse direction: adding
--     the entire asset `aAssetB` and only an appropriate amount of asset `aAssetA`
--     to preserve the invariant. Consider, for example, the equation for
--     `aBAmount`, where the underlying notion of alpha, according to the
--     terminology used in the theoretical foundations for constant factor
--     automated market makers, clearly assumes that the proportion between assets
--     will be preserved simply by restricting the amount of asset `aAssetB` that
--     is effectively added. A similar incorrect approach is taken for the
--     computation of the number of new LP tokens.
-- -}
-- addLiq :: Pool -> AddLiqParams -> Maybe AddLiqRes
-- addLiq p@Pool{..} AddLiqParams{..}
--     | unacceptableParams = Nothing
--     | otherwise = do
--         newAccount <-
--             rfrAccount <$> rmFunds alpAccount
--                 (mkRmFundsParams [ aAssetA { aAmount = 0 }
--                                  , aAssetB { aAmount =
--                                                     aAmount aAssetB - aBAmount }
--                                  ])
--         mkRes (pAAmount + aAAmount)
--               (pBAmount + aBAmount)
--               (pLiqTokens + newLiqTokens)
--               newAccount
--   where
--     unacceptableParams :: Bool
--     unacceptableParams =    aAAmount < 0
--                          || aBAmount < 0
--                          || pAAmount /= aAAmount
--                          || pBAmount /= aBAmount

--     pAName :: Currency
--     pAName = aName pAssetA

--     pAAmount :: Integer
--     pAAmount = aAmount pAssetA

--     pAssetA :: Asset
--     pAssetA = lAssetA pLiq

--     pBName :: Currency
--     pBName = aName pAssetB

--     pBAmount :: Integer
--     pBAmount = aAmount pAssetB

--     pAssetB :: Asset
--     pAssetB = lAssetB pLiq

--     aA = aName aAssetA
--     aAAmount = aAmount aAssetA
--     aAssetA = lAssetA alpLiq

--     aB = aName aAssetB
--     aBAmount = (aAAmount * pBAmount) `div` pAAmount
--     aAssetB = lAssetB alpLiq

--     newLiqTokens :: Integer
--     newLiqTokens = (aAAmount * pLiqTokens) `div` pAAmount

--     mkRes :: Integer -> Integer -> Integer -> Account -> Maybe AddLiqRes
--     mkRes a b c =
--         Just . mkAddLiqRes p {pLiq = pLiq { lAssetA = pAssetA {aAmount = a}
--                                           , lAssetB = pAssetB {aAmount = b}
--                                           }
--                              , pLiqTokens = c
--                              }

-- rmLiq :: Pool -> RmLiqParams -> Maybe RmLiqRes
-- rmLiq p@Pool{..} RmLiqParams{..}
--     | unacceptableParams = Nothing
--     |          otherwise = updAccount >>= Just . mkRmLiqRes (p {pLiq = newpLiq})
--   where
--     unacceptableParams :: Bool
--     unacceptableParams = rlpLiqTokens < 0 || rlpLiqTokens > pLiqTokens

--     newpLiq :: Liq
--     newpLiq = pLiq { lAssetA = mkAsset (aName assetA) (availableA - rmAAmount)
--                    , lAssetB = mkAsset (aName assetB) (availableB - rmBAmount)
--                    }

--     assetA :: Asset
--     assetA = lAssetA pLiq

--     availableA :: Integer
--     availableA = aAmount assetA

--     rmAAmount :: Integer
--     rmAAmount = (rlpLiqTokens * availableA) `div` pLiqTokens

--     assetB :: Asset
--     assetB = lAssetB pLiq

--     availableB :: Integer
--     availableB = aAmount assetB

--     rmBAmount :: Integer
--     rmBAmount = (rlpLiqTokens * availableB) `div` pLiqTokens

--     updAccount :: Maybe Account
--     updAccount =
--         do afr <- addFunds rlpAccount
--                            $ mkAddFundsParams [ mkAsset (aName assetA) rmAAmount
--                                               , mkAsset (aName assetB) rmBAmount
--                                               ]
--            Just $ afrAccount afr

--     newAssetA :: Asset
--     newAssetA = mkAsset (aName assetA) rmAAmount

--     newAssetB :: Asset
--     newAssetB = mkAsset (aName assetB) rmBAmount

-- swap :: Pool -> SwapParams -> Maybe SwapRes
-- swap p@Pool{..} SwapParams{..}
--     | swapAForB =
--         let payOut = ceiling $
--                         (fromIntegral $ (aAmount assetA) * (aAmount assetB))
--                             / (fromIntegral $ aAmount spAsset)
--             newPool = p {pLiq = updLiq (aAmount spAsset) (-payOut) pLiq}
--         in mkRes spAsset (mkAsset (aName $ lAssetB pLiq) payOut) newPool
--     | swapBForA =
--         let payOut = ceiling $
--                         (fromIntegral $ (aAmount assetA) * (aAmount assetB))
--                             / (fromIntegral $ aAmount spAsset)
--             newPool = p {pLiq = updLiq (-(aAmount spAsset)) payOut pLiq}
--         in mkRes spAsset (mkAsset (aName $ lAssetA pLiq) payOut) newPool
--     | otherwise = Nothing
--   where
--     swapAForB :: Bool
--     swapAForB = aName spAsset == aName (lAssetA pLiq)

--     assetA :: Asset
--     assetA = lAssetA pLiq

--     swapBForA :: Bool
--     swapBForA = aName spAsset == aName (lAssetB pLiq)

--     assetB :: Asset
--     assetB = lAssetB pLiq

--     updLiq :: Integer -> Integer -> Liq -> Liq
--     updLiq a' b' l@Liq{ lAssetA = aAsset@Asset{ aAmount = a }
--                       , lAssetB = bAsset@Asset{ aAmount = b }
--                       }
--                  =  l { lAssetA = aAsset{ aAmount = a + a' }
--                       , lAssetB = bAsset{ aAmount = b + b' }
--                       }

--     mkRes :: Asset -> Asset -> Pool -> Maybe SwapRes
--     mkRes removeAsset pay newPool =
--         updAccount spAccount removeAsset pay >>= Just . mkSwapRes newPool pay

--     updAccount :: Account -> Asset -> Asset -> Maybe Account
--     updAccount a payment payout =
--         do a' <- rfrAccount <$> (rmFunds a $ mkRmFundsParams [payment])
--            a'' <- afrAccount <$> (addFunds a' $ mkAddFundsParams [payout])
--            Just a''

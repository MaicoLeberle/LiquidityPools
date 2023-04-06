{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Business
    (   -- Actions.
      newTokens
    , initialTokens
    , createUserID
    , rmLiq
    -- , swap
    ) where

import           Data.List
import qualified Data.Tuple as T
import           System.Random

import Types


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

-- POST requests.
{-  The number of new LP tokens is given by the underlying theory--c.f.
    "Formal Specification of Constant Product (x * y = k) Market Maker Model and
    Implementation": <https://github.com/runtimeverification/verified-smart-contracts/blob/c40c98d6ae35148b76742aaaa29e6eaa405b2f93/uniswap/x-y-k.pdf>
-}
initialTokens :: Integer -> Integer -> Integer
initialTokens newA newB = floor $ sqrt $ fromInteger $ newA * newB

{-  FIX: Implement preservation of the constant factor invariant and compute the
    right number of LP tokens.
    In particular, note that we assume here that asset `aAssetA` will be added
    entirely to the liquidity of the pool, while only an appropriate amount of
    asset aAssetB will be added in order to preserve the constant factor
    invariant. In some cases, however, this may not be correct direction of
    restrictions, as we might need to proceed in the inverse direction: adding
    the entire asset `aAssetB` and only an appropriate amount of asset `aAssetA`
    to preserve the invariant. Consider, for example, the equation for
    `aBAmount`, where the underlying notion of alpha, according to the
    terminology used in the theoretical foundations for constant factor
    automated market makers, clearly assumes that the proportion between assets
    will be preserved simply by restricting the amount of asset `aAssetB` that
    is effectively added. A similar incorrect approach is taken for the
    computation of the number of new LP tokens.
-}
newTokens :: Liq -> Liq -> Integer -> Maybe Integer
newTokens Liq{ lAssetA = Asset{aName = oldAName, aAmount = oldA}
             , lAssetB = Asset{aName = oldBName, aAmount = oldB}
             }
          Liq{ lAssetA = Asset{aName = newAName, aAmount = newA}
             , lAssetB = Asset{aName = newBName, aAmount = newB}
             }
          oldTokens
    | wrongParams = Nothing
    | otherwise = Just $ (newA * oldTokens) `div` oldA
  where
    wrongParams :: Bool
    wrongParams =    newAName /= oldAName
                  || newBName /= oldBName
                  || newA < 0
                  || newB < 0

rmLiq :: Pool -> Integer -> Liq
rmLiq p@Pool{ pLiq = Liq { lAssetA = Asset{aName = a, aAmount = oldA}
                         , lAssetB = Asset{aName = b, aAmount = oldB}
                         }
            , pLiqTokens = oldTokens
            }
      tokens = mkLiq (mkAsset a rmAAmount) (mkAsset b rmBAmount)
  where
    rmAAmount :: Integer
    rmAAmount = (tokens * oldA) `div` oldTokens

    rmBAmount :: Integer
    rmBAmount = (tokens * oldB) `div` oldTokens

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

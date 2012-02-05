{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as S
import Control.Monad (liftM)
import Data.LargeWord
import Data.Digest.CityHash

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [ testGroup "simple" [
                       testProperty "cityhash64 is pure" prop_city64_pure
                     , testProperty "cityhash64WithSeed is pure" prop_city64WithSeed_pure
                     , testProperty "cityhash64WithSeeds is pure" prop_city64WithSeeds_pure
                     , testProperty "cityhash128 is pure" prop_city128_pure
                     , testProperty "cityhash128WithSeed is pure" prop_city128WithSeed_pure
                     ]
                   ]

instance Arbitrary Word128 where
  arbitrary = do
    lo <- cast `liftM` arbitrary
    hi <- cast `liftM` arbitrary
    return ((hi `shiftL` 64) .|. lo)
	    where
        cast :: Word64 -> Word128
        cast = fromIntegral


prop_city64_pure (S.pack -> xs)
  = cityHash64 xs == cityHash64 xs

prop_city64WithSeed_pure (S.pack -> xs) seed
  = cityHash64WithSeed xs seed == cityHash64WithSeed xs seed

prop_city64WithSeeds_pure (S.pack -> xs) seed0 seed1
  = cityHash64WithSeeds xs seed0 seed1 == cityHash64WithSeeds xs seed0 seed1

prop_city128_pure (S.pack -> xs)
  = cityHash128 xs == cityHash128 xs

prop_city128WithSeed_pure (S.pack -> xs) seed
  = cityHash128WithSeed xs seed == cityHash128WithSeed xs seed


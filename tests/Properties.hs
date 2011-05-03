{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Data.ByteString as S
import Data.Digest.CityHash

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [ testGroup "simple" [
                       testProperty "cityhash64 is pure" prop_city64_pure
                     , testProperty "cityhash128 is pure" prop_city128_pure
                     ]
                   ]

prop_city64_pure (S.pack -> xs)
  = cityHash64 xs == cityHash64 xs

prop_city128_pure (S.pack -> xs)
  = cityHash128 xs == cityHash128 xs

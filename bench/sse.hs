{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
module Main
       ( main -- :: IO ()
       ) where

import Data.Int
import System.IO
import Control.Monad (liftM)
import Criterion.Main hiding (run)
import Data.ByteString.Char8 as S hiding (take, readFile)
import System.IO

import Data.LargeWord
import Control.DeepSeq

import "cityhash-old" Data.Digest.CityHash as Old
import "cityhash" Data.Digest.CityHash as New


instance NFData ByteString where
instance (NFData a, NFData b) => NFData (LargeKey a b) where
	rnf (LargeKey a b) = a `seq` b `seq` ()

main :: IO ()
main = do
	h   <- openFile "/dev/urandom" ReadMode
	str <- S.hGet h 1024
	hClose h
	defaultMain [ bench "old cityHash128, 1k string"    $ nf Old.cityHash128 str
				, bench "sse4.2 cityHash128, 1k string" $ nf New.cityHash128 str
				]

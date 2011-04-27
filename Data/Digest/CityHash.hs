{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Digest.CityHash
-- Copyright   : (c) Austin Seipp 2011
-- License     : BSD3
-- 
-- Maintainer  : as@hacks.yi.org
-- Stability   : experimental
-- Portability : portable (FFI)
-- 
-- This module implements a binding to Google's CityHash family of
-- hashing functions. You can find more information here:
-- <http://code.google.com/p/cityhash/>. It implements both the 64-bit
-- and 128-bit interfaces.
-- 
-- Note that CityHash is designed to work on architectures where
-- unaligned reads have a small penalty. In practice it is only used
-- at Google on little-endian Intel/AMD CPUs it seems, and has not
-- been tested on big-endian architectures.
-- 
module Data.Digest.CityHash
( -- * Hashing values 
  cityHash64  -- :: ByteString -> Word64
, cityHash128 -- :: ByteString -> Word128
) where
import Foreign
import Foreign.C
import Control.Monad (liftM)

import Data.LargeWord

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as U

-- | Hash a value into a 64bit result.
cityHash64 :: S.ByteString -> Word64
cityHash64 bs
  = unsafePerformIO . U.unsafeUseAsCStringLen bs $ \(cstr,clen) -> do
      return $ c_CityHash64 cstr clen
{-# INLINEABLE cityHash64 #-}

-- | Hash a value into a 128bit result. Per the google documentation,
-- this is probably faster for inputs which the length is greater than,
-- say, 200 bytes. It's also used inside google for code that wants to
-- have minimal collisions.
cityHash128 :: S.ByteString -> Word128
cityHash128 bs
  = unsafePerformIO . U.unsafeUseAsCStringLen bs $ \(cstr,clen) ->
      allocaBytes w64s $ \lo -> 
        allocaBytes w64s $ \hi -> do
          c_CityHash128 cstr clen lo hi
          lo' <- fromIntegral `liftM` peek lo
          hi' <- fromIntegral `liftM` peek hi
          return $ (hi' `shiftL` 64) .|. lo'
  where w64s = sizeOf (undefined :: Word64)
{-# INLINEABLE cityHash128 #-}

--
-- FFI bindings
-- 
foreign import ccall unsafe "hs_city.h hs_CityHash64"
  c_CityHash64 :: CString -> Int -> Word64

foreign import ccall unsafe "hs_city.h hs_CityHash128"
  c_CityHash128 :: CString -> Int -> Ptr Word64 -> Ptr Word64 -> IO ()

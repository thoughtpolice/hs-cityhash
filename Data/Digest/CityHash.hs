{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Digest.CityHash
-- Copyright   : (c) Austin Seipp 2011-2012
-- License     : MIT
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
-- 
-- This module implements a binding to the CityHash family of hashing
-- functions. You can find more information here:
-- <http://code.google.com/p/cityhash/>. It implements both the 64-bit
-- and 128-bit interfaces, with seed functionality.
-- 
module Data.Digest.CityHash
      ( -- * 64-bit hashes
        cityHash64          -- :: ByteString -> Word64
      , cityHash64WithSeed  -- :: ByteString -> Word64 -> Word64
      , cityHash64WithSeeds -- :: ByteString -> Word64 -> Word64 -> Word64
        -- * 128-bit hashes
      , cityHash128         -- :: ByteString -> Word128
      , cityHash128WithSeed -- :: ByteString -> Word128 -> Word128
      ) where
import Foreign
import Foreign.C
import Control.Monad (liftM)
import System.IO.Unsafe as U (unsafePerformIO)
import Data.LargeWord

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as U

-- | Hash function for a byte array.
cityHash64 :: S.ByteString -> Word64
cityHash64 bs
  = unsafeUseBS bs $ \(cstr, clen) -> do
      return $ c_CityHash64 cstr clen
{-# INLINEABLE cityHash64 #-}

-- | Hash function for a byte array.  For convenience, a 64-bit seed is also
-- hashed into the result.
cityHash64WithSeed :: S.ByteString -> Word64 -> Word64
cityHash64WithSeed bs seed
  = unsafeUseBS bs $ \(cstr, clen) -> do
      return $ c_CityHash64WithSeed cstr clen seed
{-# INLINEABLE cityHash64WithSeed #-}

-- | Hash function for a byte array.  For convenience, two seeds are also
-- hashed into the result.
cityHash64WithSeeds :: S.ByteString -> Word64 -> Word64 -> Word64
cityHash64WithSeeds bs seed0 seed1
  = unsafeUseBS bs $ \(cstr, clen) -> do
      return $ c_CityHash64WithSeeds cstr clen seed0 seed1
{-# INLINEABLE cityHash64WithSeeds #-}


-- | Hash function for a byte array.
cityHash128 :: S.ByteString -> Word128
cityHash128 bs
  = unsafeUseBS bs $ \(cstr,clen) ->
      allocaBytes w64s $ \lo -> 
        allocaBytes w64s $ \hi -> do
          c_CityHash128 cstr clen lo hi
          lo' <- (fromIntegral `liftM` peek lo) :: IO Word128
          hi' <- (fromIntegral `liftM` peek hi) :: IO Word128
          return $ (hi' `shiftL` 64) .|. lo'
  where w64s = sizeOf (undefined :: Word64)
{-# INLINEABLE cityHash128 #-}

-- | Hash function for a byte array.  For convenience, a 128-bit seed is also
-- hashed into the result.
cityHash128WithSeed :: S.ByteString -> Word128 -> Word128
cityHash128WithSeed bs seed
  = unsafeUseBS bs $ \(cstr,clen) ->
      allocaBytes w64s $ \lo -> 
        allocaBytes w64s $ \hi -> do
          c_CityHash128WithSeed cstr clen (loHalf seed) (hiHalf seed) lo hi
          lo' <- (fromIntegral `liftM` peek lo) :: IO Word128
          hi' <- (fromIntegral `liftM` peek hi) :: IO Word128
          return $ (hi' `shiftL` 64) .|. lo'
  where w64s = sizeOf (undefined :: Word64)
{-# INLINEABLE cityHash128WithSeed #-}

-- 
-- Utils
-- 

unsafeUseBS :: S.ByteString -> ((CString, Int) -> IO a) -> a
unsafeUseBS bs = U.unsafePerformIO . U.unsafeUseAsCStringLen bs

--
-- FFI bindings
-- 

foreign import ccall unsafe "hs_city.h hs_CityHash64"
  c_CityHash64 :: CString -> Int -> Word64

foreign import ccall unsafe "hs_city.h hs_CityHash64WithSeed"
  c_CityHash64WithSeed :: CString -> Int -> Word64 -> Word64

foreign import ccall unsafe "hs_city.h hs_CityHash64WithSeeds"
  c_CityHash64WithSeeds :: CString -> Int -> Word64 -> Word64 -> Word64

foreign import ccall unsafe "hs_city.h hs_CityHash128"
  c_CityHash128 :: CString -> Int -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "hs_city.h hs_CityHash128WithSeed"
  c_CityHash128WithSeed :: CString -> Int -> Word64 -> Word64 
                        -> Ptr Word64 -> Ptr Word64 -> IO ()

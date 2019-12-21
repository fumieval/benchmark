module Main where

import Gauge.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Mason.Builder as M
import Data.Word

main = defaultMain
  [ bench "bytestring" $ nf (construct B.singleton) 0
  , bench "bytestring-builder" $ nf (BB.toLazyByteString . construct BB.word8) 0
  , bench "bytestring-builder/toStrict" $ nf (BL.toStrict . BB.toLazyByteString . construct BB.word8) 0
  , bench "fast-builder/lazy" $ nf (FB.toLazyByteString . construct FB.word8) 0
  , bench "fast-builder/strict" $ nf (FB.toStrictByteString . construct FB.word8) 0
  , bench "mason/lazy" $ nf (M.toLazyByteString . construct M.word8) 0
  , bench "mason/strict" $ nf (M.toStrictByteString . construct M.word8) 0
  ]

construct :: Monoid a => (Word8 -> a) -> Word8 -> a
construct p = \x -> p x <> p (x + 1) <> p (x + 2) <> p (x + 3)
{-# INLINE construct #-}

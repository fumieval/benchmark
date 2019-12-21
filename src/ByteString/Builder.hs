{-# OPTIONS -ddump-simpl -dsuppress-all -ddump-to-file #-}
{-# LANGUAGE CPP #-}
module Main where

import Data.Word
import Gauge.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Lazy as BL
import qualified Mason.Builder as M

#define EXPR(p) (\x -> p x <> p (x + 1) <> p (x + 2) <> p (x + 3) <> p (x + 4) <> p (x + 5) <> p (x + 6) <> p (x + 7))

main = defaultMain
  [ bench "bytestring" $ nf (EXPR(B.singleton)) 0
  , bench "bytestring-builder" $ nf (BB.toLazyByteString . EXPR(BB.word8)) 0
  , bench "bytestring-builder/toStrict" $ nf (BL.toStrict . BB.toLazyByteString . EXPR(BB.word8)) 0
  , bench "fast-builder/lazy" $ nf (FB.toLazyByteString . EXPR(FB.word8)) 0
  , bench "fast-builder/strict" $ nf (FB.toStrictByteString . EXPR(FB.word8)) 0
  , bench "mason/lazy" $ nf (M.toLazyByteString . EXPR(M.word8)) 0
  , bench "mason/strict" $ nf (M.toStrictByteString . EXPR(M.word8)) 0
  ]

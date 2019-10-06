{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Gauge.Main

sTexts :: [T.Text]
sTexts = T.words "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

group :: [T.Text] -> [Benchmark]
group xs =
  [ bench "Text-encode-Builder-LBS-ByteString" $ whnf (BL.toStrict . BB.toLazyByteString . foldMap (BB.byteString . TE.encodeUtf8)) xs
  , bench "Text-Builder-LBS-ByteString" $ whnf (BL.toStrict . BB.toLazyByteString . foldMap TE.encodeUtf8Builder) xs
  , bench "Text-encode-ByteString" $ whnf (foldMap TE.encodeUtf8) xs
  ]

main :: IO ()
main = defaultMain
  [ bgroup "small" $ group sTexts
  , bgroup "large" $ group $ map (T.concat . replicate 50) sTexts
  ]

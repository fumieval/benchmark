{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Gauge.Main

sTexts :: [T.Text]
sTexts = T.words "祇園精舎の鐘の声 諸行無常の響きあり 沙羅双樹の花の色 盛者必衰の理をあらはす 奢れる人も久からず ただ春の夜の夢のごとし 猛き者も遂にはほろびぬ 偏ひとへに風の前の塵におなじ"

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

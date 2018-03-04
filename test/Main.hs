{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Benchmarking.VectorSum (createData)
import qualified Benchmarking.VectorSum.CheckedStFromBack as CheckedStFromBack
import qualified Benchmarking.VectorSum.CheckedStFromFront as CheckedStFromFront
import qualified Benchmarking.VectorSum.FoldZip as FoldZip
import qualified Benchmarking.VectorSum.FoldZipWith6 as FoldZipWith6
import qualified Benchmarking.VectorSum.RecurseZip as RecurseZip
import qualified Benchmarking.VectorSum.RecurseZipWithN as RecurseZipWithN
import qualified Benchmarking.VectorSum.UncheckedStFromBack as UncheckedStFromBack
import qualified Benchmarking.VectorSum.UncheckedStFromBackBailEmpty as UncheckedStFromBackBailEmpty
import qualified Benchmarking.VectorSum.UncheckedStFromBackModify as UncheckedStFromBackModify
import qualified Benchmarking.VectorSum.UncheckedStFromFront as UncheckedStFromFront
import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Test.Hspec

main :: IO ()
main = hspec $ do
  it "all implementations give the same result (Data.Vector)" $ do
    checkSumsEqual @V.Vector vectorData
  it "all implementations give the same result (Data.Vector.Storable)" $ do
    checkSumsEqual @VS.Vector vectorData
  it "all implementations give the same result (Data.Vector.Unboxed)" $ do
    checkSumsEqual @VU.Vector vectorData
  where
    vectorData :: VG.Vector v Double => NonEmpty (v Double)
    vectorData = createData 100 100

    vsums :: VG.Vector v Double => [NonEmpty (v Double) -> v Double]
    vsums = [ FoldZip.vsum, RecurseZip.vsum, RecurseZipWithN.vsum
            , FoldZipWith6.sum
            , UncheckedStFromBack.vsum, UncheckedStFromFront.vsum
            , UncheckedStFromBackBailEmpty.vsum, UncheckedStFromBackModify.vsum
            , CheckedStFromBack.vsum, CheckedStFromFront.vsum
            ]

    checkSumsEqual
      :: (Show (v Double), Eq (v Double), VG.Vector v Double)
      => NonEmpty (v Double) -> Expectation
    checkSumsEqual vs = case fmap ($ vs) vsums of
      [] -> pure ()
      h : ss -> mapM_ (`shouldBe` h) ss

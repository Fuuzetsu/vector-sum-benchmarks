{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main (main) where

import           Benchmarking.VectorSum (createData)
import qualified Benchmarking.VectorSum.CheckedStFromBack as CheckedStFromBack
import qualified Benchmarking.VectorSum.CheckedStFromFront as CheckedStFromFront
import qualified Benchmarking.VectorSum.FoldZip as FoldZip
import qualified Benchmarking.VectorSum.RecurseZip as RecurseZip
import qualified Benchmarking.VectorSum.RecurseZipWithN as RecurseZipWithN
import qualified Benchmarking.VectorSum.UncheckedStFromBack as UncheckedStFromBack
import qualified Benchmarking.VectorSum.UncheckedStFromBackBailEmpty as UncheckedStFromBackBailEmpty
import qualified Benchmarking.VectorSum.UncheckedStFromBackModify as UncheckedStFromBackModify
import qualified Benchmarking.VectorSum.UncheckedStFromFront as UncheckedStFromFront
import           Control.DeepSeq (NFData)
import           Data.List.NonEmpty
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Weigh

-- | A vsum over some vector type tagged with a name.
data VsumBenchmark v = VsumBenchmark
  { vsum :: !(NonEmpty (v Double) -> v Double)
  , vsumName :: !String
  }

vectorGroup
  :: forall v. (NFData (v Double), VG.Vector v Double)
  => -- | Name of the benchmark and vector type
     String
     -- | Pairs of (number of vectors, length of vectors)
  -> [(Int, Int)]
     -- | Functions to benchmark
  -> [VsumBenchmark v]
  -> Weigh ()
vectorGroup vecGroupName sizes fs =
  sequence_ [ mkBenchmark s f | s <- sizes, f <- fs ]
  where
    mkBenchmark :: (Int, Int) -> VsumBenchmark v -> Weigh ()
    mkBenchmark (vs, ls) b =
      let wName = Prelude.concat [ vecGroupName, "/"
                                 , show vs <> "x" <> show ls
                                 , "/", vsumName b ]
      in func wName (vsum b) (createData vs ls)

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Live, Check, Max]
  vectorGroup "Data.Vector" sizes (benchFuncs @V.Vector)
  vectorGroup "Data.Vector.Storable" sizes (benchFuncs @VS.Vector)
  vectorGroup "Data.Vector.Unboxed" sizes (benchFuncs @VU.Vector)
  where
    sizes :: [(Int, Int)]
    sizes =
      [ (1, 1)
      , (1, 100)
      , (1, 1000)
      , (1, 10000)
      , (1, 100000)
      , (1, 1000000)
      , (100, 1)
      , (1000, 1)
      , (10000, 1)
      , (100000, 1)
      , (1000000, 1)
      , (200, 200)
      , (500, 500)
      , (1000, 1000)
      , (5000, 5000)
      ]

    benchFuncs :: VG.Vector v Double => [VsumBenchmark v]
    benchFuncs =
      [ VsumBenchmark { vsum = FoldZip.vsum
                      , vsumName = "FoldZip" }
      , VsumBenchmark { vsum = RecurseZip.vsum
                      , vsumName = "RecurseZip" }
      , VsumBenchmark { vsum = RecurseZipWithN.vsum
                      , vsumName = "RecurseZipWithN" }
      , VsumBenchmark { vsum = UncheckedStFromBack.vsum
                      , vsumName = "UncheckedStFromBack" }
      , VsumBenchmark { vsum = UncheckedStFromBackModify.vsum
                      , vsumName = "UncheckedStFromBackModify" }
      , VsumBenchmark { vsum = UncheckedStFromBackBailEmpty.vsum
                      , vsumName = "UncheckedStFromBackBailEmpty" }
      , VsumBenchmark { vsum = UncheckedStFromFront.vsum
                      , vsumName = "UncheckedStFromFront" }
      , VsumBenchmark { vsum = CheckedStFromBack.vsum
                      , vsumName = "CheckedStFromBack" }
      , VsumBenchmark { vsum = CheckedStFromFront.vsum
                      , vsumName = "CheckedStFromFront" }
      ]

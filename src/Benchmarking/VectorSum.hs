{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum (createData) where

import           Data.List.NonEmpty
import qualified Data.Vector.Generic as VG

-- | Creates data suitable for various vsum implementations.
createData
  :: VG.Vector v Double
  => Int -- ^ Number of vectors
  -> Int -- ^ Length of vectors
  -> NonEmpty (v (Double))
createData vs l = Data.List.NonEmpty.fromList $
  Prelude.map (VG.generate l . mult) [1 .. vs]
  where
    mult :: Int -> Int -> Double
    mult x y = fromIntegral (x * y)

{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum.FoldZip (vsum) where

import           Data.List (foldl')
import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Fold a (+) zip with first vector as inital value.
vsum :: VG.Vector v Double => NonEmpty (v Double) -> v Double
vsum (v :| vs) = foldl' (VG.zipWith (+)) v vs
{-# INLINABLE vsum #-}
{-# SPECIALISE vsum :: NonEmpty (V.Vector Double) -> V.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VS.Vector Double) -> VS.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VU.Vector Double) -> VU.Vector Double #-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum.RecurseZip (vsum) where

import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Similar to FoldZip but use explicit recursion for the zips.
vsum :: VG.Vector v Double => NonEmpty (v Double) -> v Double
vsum (v :| v1 : vs) = vsum (VG.zipWith (+) v v1 :| vs)
vsum (v :| []) = v
{-# INLINABLE vsum #-}
{-# SPECIALISE vsum :: NonEmpty (V.Vector Double) -> V.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VS.Vector Double) -> VS.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VU.Vector Double) -> VU.Vector Double #-}

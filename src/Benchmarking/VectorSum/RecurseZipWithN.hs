{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum.RecurseZipWithN (vsum) where

import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Matches on up to 6 vectors and uses zipWithN to consume as many
-- of the as we can.
vsum :: VG.Vector v Double => NonEmpty (v Double) -> v Double
vsum (v0 :| v1 : v2 : v3 : v4 : v5 : vs) =
  vsum (VG.zipWith6 (\e0 e1 e2 e3 e4 e5 -> e0 + e1 + e2 + e3 + e4 + e5) v0 v1 v2 v3 v4 v5 :| vs)
vsum (v0 :| v1 : v2 : v3 : v4 : vs) =
  vsum (VG.zipWith5 (\e0 e1 e2 e3 e4 -> e0 + e1 + e2 + e3 + e4) v0 v1 v2 v3 v4 :| vs)
vsum (v0 :| v1 : v2 : v3 : vs) =
  vsum (VG.zipWith4 (\e0 e1 e2 e3 -> e0 + e1 + e2 + e3) v0 v1 v2 v3 :| vs)
vsum (v0 :| v1 : v2 : vs) =
  vsum (VG.zipWith3 (\e0 e1 e2 -> e0 + e1 + e2) v0 v1 v2 :| vs)
vsum (v0 :| v1 : vs) =
  vsum (VG.zipWith (+) v0 v1 :| vs)
vsum (v0 :| []) = v0
{-# INLINABLE vsum #-}
{-# SPECIALISE vsum :: NonEmpty (V.Vector Double) -> V.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VS.Vector Double) -> VS.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VU.Vector Double) -> VU.Vector Double #-}

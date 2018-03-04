{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum.UncheckedStFromFront (vsum) where

import           Control.Monad
import qualified Control.Monad.ST as ST
import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VG hiding (length)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Go through vectors one by one and mutate an accumulating vector
-- in place. Uses unsafe operations, performs no bounds checks.
-- Vectors are traversed from the front.
vsum :: VG.Vector v Double => NonEmpty (v Double) -> v Double
vsum (v :| vs) = ST.runST $ do
  vec <- VG.thaw v
  let vlen = VG.length v
  forM_ vs $ \v ->
    let go n | n == vlen = pure ()
        go n = do
          VG.unsafeModify vec (+ VG.unsafeIndex v n) n
          go (n + 1)
    in go 0
  VG.unsafeFreeze vec
{-# INLINABLE vsum #-}
{-# SPECIALISE vsum :: NonEmpty (V.Vector Double) -> V.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VS.Vector Double) -> VS.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VU.Vector Double) -> VU.Vector Double #-}

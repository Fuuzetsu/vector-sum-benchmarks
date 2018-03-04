{-# LANGUAGE FlexibleContexts #-}
module Benchmarking.VectorSum.UncheckedStFromBackModify (vsum) where

import           Control.Monad
import           Data.List.NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VG hiding (modify, length)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Go through vectors one by one and mutate an accumulating vector
-- in place. Uses unsafe operations, performs no bounds checks.
-- Vectors are traversed from the back. Uses 'VG.modify' to hopefully
-- prevent an unnecessary vector copy when possible
vsum :: VG.Vector v Double => NonEmpty (v Double) -> v Double
vsum (v0 :| vs) = VG.modify (\vec -> forM_ vs $ \v ->
  let go 0 = pure ()
      go n = do
        let n1 = n - 1
        VG.unsafeModify vec (+ VG.unsafeIndex v n1) n1
        go n1
  in go vlen) v0
  where
    vlen = VG.length v0
{-# INLINABLE vsum #-}
{-# SPECIALISE vsum :: NonEmpty (V.Vector Double) -> V.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VS.Vector Double) -> VS.Vector Double #-}
{-# SPECIALISE vsum :: NonEmpty (VU.Vector Double) -> VU.Vector Double #-}

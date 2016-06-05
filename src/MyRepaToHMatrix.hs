{-# LANGUAGE FlexibleContexts #-}
module MyRepaToHMatrix ( repaToMatrix, matrixToRepa ) where

import Numeric.LinearAlgebra

import Foreign.Storable
import Data.Vector.Storable
import qualified Data.Vector.Generic as GV

import qualified Data.Array.Repa as RA
import qualified Data.Array.Repa.Repr.Unboxed as RV

repaToMatrix :: (Storable e, GV.Vector Vector e, RV.Unbox e) => RV.Array RV.U RA.DIM2 e -> Matrix e
repaToMatrix a = let (RA.Z RA.:. _ RA.:. c) = RA.extent a
   in reshape c $ convert $ RV.toUnboxed a

matrixToRepa :: (Storable e, GV.Vector Vector e, RV.Unbox e, Element e) => Matrix e -> RV.Array RV.U RA.DIM2 e
matrixToRepa m = let (r, c) = (rows m, cols m)
   in RV.fromUnboxed (RA.Z RA.:. r RA.:. c) $ convert $ flatten m

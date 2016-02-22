{-# LANGUAGE FlexibleContexts #-}
module MyRepaToHMatrix ( repaToMatrix, matrixToRepa ) where

import qualified Data.Packed.Vector as HV
import qualified Data.Packed.Matrix as HM

import Foreign.Storable

import Data.Vector.Storable
import qualified Data.Vector.Generic as GV

import qualified Data.Array.Repa as RA
import qualified Data.Array.Repa.Repr.Unboxed as RV

repaToMatrix :: (Storable e, GV.Vector HV.Vector e, RV.Unbox e) => RV.Array RV.U RA.DIM2 e -> HM.Matrix e
repaToMatrix a = let (RA.Z RA.:. _ RA.:. c) = RA.extent a
   in HM.reshape c $ convert $ RV.toUnboxed a

matrixToRepa :: (Storable e, GV.Vector HV.Vector e, RV.Unbox e, HM.Element e) => HM.Matrix e -> RV.Array RV.U RA.DIM2 e
matrixToRepa m = let (r, c) = (HM.rows m, HM.cols m)
   in RV.fromUnboxed (RA.Z RA.:. r RA.:. c) $ convert $ HM.flatten m

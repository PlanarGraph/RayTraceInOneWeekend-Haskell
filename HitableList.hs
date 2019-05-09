{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HitableList where

import Hitable
import Ray
import Sphere

data HitableList a where
    MkHL :: forall a. Hitable a => [a] -> HitableList a

hitL :: HitableList a -> Ray -> Double -> Double -> Maybe HitRecord
hitL (MkHL list) r tMin tMax = fst $ foldl findClosest (Nothing, tMax) list
  where
    findClosest :: Hitable a => (Maybe HitRecord, Double) -> a -> (Maybe HitRecord, Double)
    findClosest b@(hr, closest) hitable =
      let x = hit hitable r tMin closest in
          case x of
               Nothing -> b
               Just nhr -> (Just nhr, t nhr)

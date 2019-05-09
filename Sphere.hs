{-# LANGUAGE ScopedTypeVariables #-}

module Sphere where

import Vect3
import Ray
import Hitable

data Sphere = MkSphere {
    center :: Vect3,
    radius :: Double
}

discriminant :: Sphere -> Ray -> Vect3
discriminant (MkSphere center radius) (MkRay origin direction) =
  let oc = origin - center
      a = dot direction direction
      b = dot oc direction
      c = (dot oc oc) - (radius * radius) in
      (a,b,c)

instance Hitable Sphere where
  hit s@(MkSphere center radius) r@(MkRay origin direction) tMin tMax =
    let (a, b, c) = discriminant s r
        temp1 = ((-b) - sqrt (b * b - a * c)) / a
        temp2 = ((-b) + sqrt (b * b - a * c)) / a in
        if (b * b - a * c) > 0 then
           case (temp1 < tMax && temp1 > tMin, temp2 < tMax && temp2 > tMin) of
                (True, _) -> Just $ updateRec temp1
                (_, True) -> Just $ updateRec temp2
                (_, _) -> Nothing
        else Nothing
    where
      updateRec :: Double -> HitRecord
      updateRec up =
        let np = pointAtParameter r up
            nnormal = mO (/radius) (np - center) in
            (MkHR up np nnormal)

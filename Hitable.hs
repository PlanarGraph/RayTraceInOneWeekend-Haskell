{-# LANGUAGE ScopedTypeVariables #-}
module Hitable where

import Vect3
import Ray
import System.Random

data HitRecord = MkHR {
    t :: Double,
    p :: Vect3,
    normal :: Vect3,
    mat :: Material
}

data Material =
    Lambertian Vect3
  | Metal Vect3 Double

data Hitable =
    Sphere Vect3 Double Material

discriminant :: Hitable -> Ray -> Vect3
discriminant (Sphere center radius _) (MkRay origin direction) =
  let oc = origin - center
      a = dot direction direction
      b = dot oc direction
      c = (dot oc oc) - (radius * radius) in
      (a,b,c)

hit :: Hitable -> Ray -> Double -> Double -> Maybe HitRecord
hit s@(Sphere center radius material) r@(MkRay origin direction) tMin tMax =
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
          (MkHR up np nnormal material)

randomInUnitSphere :: IO Vect3
randomInUnitSphere = do
  x <- randomRIO (0.0, 0.9999999999999999)
  y <- randomRIO (0.0, 0.9999999999999999)
  z <- randomRIO (0.0, 0.9999999999999999)
  let p = mO (*2) (x,y,z) - (1,1,1)
  if sqrLen p >= 1.0
     then randomInUnitSphere
     else return p

reflect :: Vect3 -> Vect3 -> Vect3
reflect v n = 
  let mult = 2 * dot v n in
      v - mO (*mult) n

scatter :: HitRecord -> Ray -> IO (Vect3, Ray, Bool)
scatter (MkHR t p normal (Lambertian albedo)) (MkRay origin direction) = do
  rius <- randomInUnitSphere
  let target = p + normal + rius
      scattered = MkRay p (target - p)
      attenuation = albedo in
      return (attenuation, scattered, True)
scatter (MkHR t p normal (Metal albedo fuzz)) (MkRay origin direction) = do
  rius <- randomInUnitSphere
  let reflected = reflect (unit direction) normal
      scattered = MkRay p (reflected + mO (*fuzz) rius)
      attenuation = albedo 
      bool = dot reflected normal > 0 in
      return (attenuation, scattered, bool)

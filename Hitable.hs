{-# LANGUAGE ScopedTypeVariables #-}
module Hitable where

import           Ray
import           System.Random
import           Vect3

data HitRecord = MkHR {
    t      :: Double,
    p      :: Vect3,
    normal :: Vect3,
    mat    :: Material
}

data Material =
    Lambertian Vect3
  | Metal Vect3 Double
  | Dielectric Double

data Hitable =
    Sphere Vect3 Double Material

discriminant :: Hitable -> Ray -> Vect3
discriminant (Sphere center radius _) (MkRay origin direction) =
  let oc = origin - center
      a = dot direction direction
      b = dot oc direction
      c = dot oc oc - (radius * radius) in
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
              (_, _)    -> Nothing
      else Nothing
  where
    updateRec :: Double -> HitRecord
    updateRec up =
      let np = pointAtParameter r up
          nnormal = mO (/radius) (np - center) in
          MkHR up np nnormal material

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

refract :: Vect3 -> Vect3 -> Double -> Maybe Vect3
refract v n nt =
  let uv = unit v
      dt = dot uv n
      discriminant = 1.0 - (nt**2) * (1 - dt**2)
      a = mO (*nt) (uv - mO (*dt) n)
      b = mO (* sqrt discriminant) n in
      if discriminant > 0
         then Just (a - b)
         else Nothing

schlick :: Double -> Double -> Double
schlick cosine refIdx =
  let r0 = ((1 - refIdx) / (1 + refIdx)) ** 2 in
      r0 + (1 - r0) * (1 - cosine) ** 5

scatter :: HitRecord -> Ray -> IO (Vect3, Ray, Bool)
scatter (MkHR t p normal (Lambertian attenuation)) (MkRay origin direction) = do
  rius <- randomInUnitSphere
  let target = p + normal + rius
      scattered = MkRay p (target - p) in
      return (attenuation, scattered, True)

scatter (MkHR t p normal (Metal attenuation fuzz)) (MkRay origin direction) = do
  rius <- randomInUnitSphere
  let reflected = reflect (unit direction) normal
      scattered = MkRay p (reflected + mO (*fuzz) rius)
      bool = dot reflected normal > 0 in
      return (attenuation, scattered, bool)

scatter (MkHR t p normal (Dielectric refIdx)) (MkRay origin direction) =
  let reflected = reflect direction normal in
      case refract direction outNorm nt of
           Nothing -> return ((1.0, 1.0, 1.0), MkRay p reflected, True)
           Just refracted -> do
             rnd <- randomRIO (0.0, 0.9999999999999999)
             if rnd < schlick cosine refIdx
                then return ((1.0, 1.0, 1.0), MkRay p reflected, True)
                else return ((1.0, 1.0, 1.0), MkRay p refracted, True)
  where
    (outNorm, nt, cosine) = if dot direction normal > 0
                               then let cosi = (refIdx * dot direction normal) / len direction in
                                        (mO (*(-1)) normal, refIdx, cosi)
                               else let cosi = ((-1) * dot direction normal) / len direction in
                                        (normal, 1.0 / refIdx, cosi)

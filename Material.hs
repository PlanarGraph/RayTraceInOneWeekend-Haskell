module Material where

import Ray
import Vect3
import Hitable
import Control.Monad.Trans.Maybe
import System.Random
import Control.Monad.IO.Class

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
scatter (MkHR t p normal (Metal albedo)) (MkRay origin direction) = do
  let reflected = reflect (unit direction) normal
      scattered = MkRay p reflected
      attenuation = albedo 
      bool = dot reflected normal > 0 in
      return (attenuation, scattered, bool)

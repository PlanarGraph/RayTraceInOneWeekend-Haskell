module Camera where

import Vect3
import Ray
import System.Random

data Camera = MkCamera {
    lowerLeftCorner :: Vect3,
    horizontal :: Vect3,
    vertical :: Vect3,
    origin :: Vect3,
    u :: Vect3,
    v :: Vect3,
    w :: Vect3,
    lensRadius :: Double
}

randomInUnitDisk :: IO Vect3
randomInUnitDisk = do
  x <- randomRIO (0.0, 0.9999999999999999)
  y <- randomRIO (0.0, 0.9999999999999999)
  let p = mO (*2) (x,y,0) - (1,1,0)
  if dot p p >= 1.0
     then randomInUnitDisk
     else return p

makeCamera :: Vect3 -> Vect3 -> Vect3 -> Double -> Double -> Double -> Double -> Camera
makeCamera origin lookat vup vfov aspect aperture focusDist =
  let lensRadius = aperture / 2
      theta = vfov * pi / 180
      halfHeight = tan (theta / 2)
      halfWidth = aspect * halfHeight
      w = unit (origin - lookat)
      u = unit (cross vup w)
      v = cross w u
      lowerLeftCorner = origin - mO (*(halfWidth * focusDist)) u - mO (*(halfHeight * focusDist)) v - mO (*focusDist) w
      horizontal = mO (\x -> 2 * halfWidth * focusDist * x) u
      vertical = mO (\x -> 2 * halfHeight * focusDist * x) v in
      MkCamera lowerLeftCorner horizontal vertical origin w u v lensRadius

getRay :: Camera -> Double -> Double -> IO Ray
getRay (MkCamera llc ho ve or u v w lr) s t = do
    rnd <- randomInUnitDisk
    let rd = mO (*lr) rnd
        offset = mO (*(getX rd)) u + mO (*(getY rd)) v in
        return $ MkRay (or + offset) (llc + mO (*s) ho + mO (*t) ve - or - offset)

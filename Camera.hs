module Camera where

import Vect3
import Ray

data Camera = MkCamera {
    lowerLeftCorner :: Vect3,
    horizontal :: Vect3,
    vertical :: Vect3,
    origin :: Vect3
}

defaultCamera :: Camera
defaultCamera =
  MkCamera (-2.0, -1.0, -1.0) (4.0, 0.0, 0.0) (0.0, 2.0, 0.0) (0.0, 0.0, 0.0)

getRay :: Camera -> Double -> Double -> Ray
getRay (MkCamera llc ho ve or) u v =
    MkRay or ((llc + mO (*u) ho + mO (*v) ve) - or)

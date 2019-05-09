module Hitable where

import Vect3
import Ray

data HitRecord = MkHR {
    t :: Double,
    p :: Vect3,
    normal :: Vect3
}

class Hitable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

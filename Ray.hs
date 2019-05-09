module Ray where

import Vect3

data Ray = MkRay {
    origin :: Vect3,
    direction :: Vect3
}

pointAtParameter :: Ray -> Double -> Vect3
pointAtParameter (MkRay origin direction) y = origin + direction * (y,y,y)

{-# LANGUAGE FlexibleInstances #-}

module Vect3 where

type Vect3 = (Double, Double, Double)

getX :: Vect3 -> Double
getX (x,_,_) = x

getY :: Vect3 -> Double
getY (_,y,_) = y

getZ :: Vect3 -> Double
getZ (_,_,z) = z

dot :: Vect3 -> Vect3 -> Double
dot (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vect3 -> Vect3 -> Vect3
cross (x1,y1,z1) (x2,y2,z2) =
  let v1 = y1 * z2 - z1 * y2
      v2 = (-1) * (x1 * z2 - z1 * x2)
      v3 = x1 * y2 - y1 * x2 in
      (v1,v2,v3)

sqrLen :: Vect3 -> Double
sqrLen (x, y, z) = x^2 + y^2 + z^2

len :: Vect3 -> Double
len = sqrt . sqrLen

unit :: Vect3 -> Vect3
unit v = let l = len v in
             mO (/l) v

makeUnitVect :: Vect3 -> Vect3
makeUnitVect x = let k = 1.0 / len x in
                     mO (*k) x

aO :: (Double -> Double -> Double) -> Vect3 -> Vect3 -> Vect3
aO f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

mO :: (Double -> a) -> Vect3 -> (a,a,a)
mO f (x1,y1,z1) =(f x1, f y1, f z1)

instance Num Vect3 where
    (+) = aO (+)
    (-) = aO (-)
    (*) = aO (*)
    abs = mO abs
    fromInteger x = (0,0,0)
    signum = mO signum

instance Fractional Vect3 where
    fromRational x = (fromRational x,fromRational x,fromRational x)
    (/) = aO (/)


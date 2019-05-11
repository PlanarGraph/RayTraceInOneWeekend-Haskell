{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module HitableList where

import           Hitable
import           Ray
import           Vect3
import           Control.Monad
import           Data.Maybe
import           System.Random

type HitableList = [Hitable]


genLambertian :: IO Material
genLambertian = do 
  x1 <- randomRIO (0.0, 0.9999999999999999) 
  x2 <- randomRIO (0.0, 0.9999999999999999)
  y1 <- randomRIO (0.0, 0.9999999999999999)
  y2 <- randomRIO (0.0, 0.9999999999999999)
  z1 <- randomRIO (0.0, 0.9999999999999999)
  z2 <- randomRIO (0.0, 0.9999999999999999)
  return $ Lambertian (x1 * x2, y1 * y2, z1 * z2)

genMetal :: IO Material
genMetal = do
  x <- randomRIO (0.0, 0.9999999999999999)
  y <- randomRIO (0.0, 0.9999999999999999)
  z <- randomRIO (0.0, 0.9999999999999999)
  f <- randomRIO (0.0, 0.9999999999999999)
  return $ Metal (0.5 * (1 + x), 0.5 * (1+y), 0.5 *(1+z)) (0.5 * f)

genSphere :: (Int, Int) -> IO (Maybe Hitable)
genSphere (a, b) = do
  chooseMat <- randomRIO (0.0, 0.9999999999999999) :: IO Double
  x <- randomRIO (0.0, 0.9999999999999999)
  z <- randomRIO (0.0, 0.9999999999999999)
  let center = (fromIntegral a + 0.9 * x, 0.2, fromIntegral b + 0.9 * z)
  if len (center - (4, 0.2, 0)) > 0.9 
     then case (chooseMat < 0.8, chooseMat < 0.95) of
               (True, _) -> do 
                 lambertian <- genLambertian
                 return $ Just $ Sphere center 0.2 lambertian
               (_, True) -> do
                 metal <- genMetal
                 return $ Just $ Sphere center 0.2 metal
               (_, _) -> return $ Just $ Sphere center 0.2 (Dielectric 1.5)
     else return Nothing

randomWorld :: IO HitableList
randomWorld = 
  let begin = [Sphere (0, -1000, 0) 1000 (Lambertian (0.5, 0.5, 0.5))]
      end = [ Sphere (0, 1, 0) 1.0 (Dielectric 1.5)
            , Sphere (-4, 1, 0) 1.0 (Lambertian (0.4, 0.2, 0.1))
            , Sphere (4, 1, 0) 1.0 (Metal (0.7, 0.6, 0.5) 0.0)] in do
  wld <- forM [ (a,b) | a <- [(-4)..3], b <- [(-4)..3] ] genSphere
  return $ begin <> catMaybes wld <> end 

hitL :: HitableList -> Ray -> Double -> Double -> Maybe HitRecord
hitL list r tMin tMax = fst $ foldl findClosest (Nothing, tMax) list
  where
    findClosest :: (Maybe HitRecord, Double) -> Hitable -> (Maybe HitRecord, Double)
    findClosest b@(hr, closest) hitable =
      let x = hit hitable r tMin closest in
          case x of
               Nothing  -> b
               Just nhr -> (Just nhr, t nhr)

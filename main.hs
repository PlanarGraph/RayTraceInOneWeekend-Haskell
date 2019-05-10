{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Vect3
import Ray
import Hitable
import HitableList
import Camera
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import System.Random

color :: Ray -> HitableList -> Int -> IO Vect3
color r@(MkRay origin direction) world depth =
  case hitL world r 0.001 9.9e999 of
       Nothing -> let unitDir = unit direction
                      t = 0.5 * (getY unitDir) + 1.0 in
                      return $ mO (*(1.0-t)) (1.0, 1.0, 1.0) + mO (*t) (0.5, 0.7, 1.0)
       Just rec@(MkHR t p normal material) -> do
         (attenuation, scattered, bool) <- scatter rec r
         if depth < 50 && bool
            then do
              col <- color scattered world (depth+1)
              return $ attenuation * col
         else return (0,0,0)

printVect :: Vect3 -> T.Text
printVect v = let (x,y,z) = mO floor v in
    T.pack (show x) <> " " <> T.pack (show y) <> " " <> T.pack (show z)

getSamples :: Int -> Int -> Int -> Int -> Camera -> HitableList ->
              Vect3 -> Int ->  IO Vect3
getSamples i j nx ny camera world col a = do
  r1 <- randomRIO (0.0, 0.9999999999999999)
  r2 <- randomRIO (0.0, 0.9999999999999999)
  let u = (fromIntegral i + r1) / (fromIntegral nx)
      v = (fromIntegral j + r2) / (fromIntegral ny)
      r = getRay camera u v
  c1 <- color r world 0
  return (col + c1)

main :: IO ()
main =
  let nx = 200 :: Int
      ny = 100 :: Int
      ns = 100 :: Int
      header = ["P3", T.pack (show nx), T.pack (show ny), "255"]
      camera = defaultCamera
      world = [ Sphere (0, 0, -1) 0.5 (Lambertian (0.1, 0.2, 0.5))
              , Sphere (0, -100.5, -1) 100 (Lambertian (0.8, 0.8, 0.0))
              , Sphere (1, 0, -1) 0.5 (Metal (0.8, 0.6, 0.2) 1.0)
              , Sphere (-1, 0, -1) 0.5 (Dielectric 1.5) 
              , Sphere (-1, 0, -1) (-0.45) (Dielectric 1.5)] in do
      forM_ header TIO.putStrLn
      forM_ [(j,i) | j <- [(ny-1),(ny-2)..0], i <- [0..(nx-1)]] $ \(j,i) -> do
         col <- foldM (getSamples i j nx ny camera world) (0, 0, 0) [0..(ns-1)]
         TIO.putStrLn $ printVect $ mO (\x -> (*) 255.99 $ sqrt $ x / (fromIntegral ns)) col

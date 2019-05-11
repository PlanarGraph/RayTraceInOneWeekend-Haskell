module Main where

import           Control.Monad.Trans.Reader
import           Hitable
import           Scene
import           HitableList
import qualified Data.ByteString.Char8 as BC
import Control.Monad

main :: IO ()
main = do
  wld <- randomWorld
  --let wld =
  --      [ Sphere (0 , 0     , -1) 0.5     (Lambertian (0.1, 0.2, 0.5))
  --      , Sphere (0 , -100.5, -1) 100     (Lambertian (0.8, 0.8, 0.0))
  --      , Sphere (1 , 0     , -1) 0.5     (Metal (0.8, 0.6, 0.2) 1.0)
  --      , Sphere (-1, 0     , -1) 0.5     (Dielectric 1.5)
  --      , Sphere (-1, 0     , -1) (-0.45) (Dielectric 1.5)
  --      ]
  let scene = makeScene 1200 800 10 (13, 2, 3) (0, 0, 0) 20 0.1 wld  
  x <- runReaderT renderScene scene
  forM_ x BC.putStrLn

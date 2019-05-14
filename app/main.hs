module Main where

import           Control.Monad.Trans.Reader
import           Control.Monad
import           Hitable
import           Scene
import           Vect3
import           HitableList
import qualified Data.ByteString.Char8         as BC

main :: IO ()
main = do
  wld <- randomWorld
  let scene = makeScene 400 200 100 (13, 2, 3) (0, 0, 0) 20 0.1 wld
  pics <- runReaderT renderScene scene
  let pic = map printVect pics
  BC.writeFile "out.ppm" (BC.unlines $ header scene <> pic)

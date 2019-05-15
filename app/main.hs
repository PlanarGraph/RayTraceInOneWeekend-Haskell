module Main where

import           Control.Monad.Trans.Reader
import           Control.Monad
import           Hitable
import           Scene
import           Vect3
import           HitableList
import qualified Data.ByteString.Char8         as BC
import           System.Environment

loadWorld :: [String] -> IO HitableList
loadWorld [] = randomWorld
loadWorld (x:_) = do
  w <- readWorld x
  case w of
       Just wld -> return wld
       Nothing -> randomWorld

main :: IO ()
main = do
  args <- getArgs 
  wld <- loadWorld args
  let scene = makeScene 200 100 100 (13, 2, 3) (0, 0, 0) 20 0.1 wld
  pics <- runReaderT renderScene scene
  let pic = map printVect pics
  BC.writeFile "out.ppm" (BC.unlines $ header scene <> pic)

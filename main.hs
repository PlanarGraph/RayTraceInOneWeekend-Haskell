module Main where

import           Control.Monad.Trans.Reader
import           Control.Monad
import           Hitable
import           Scene
import           Vect3
import           HitableList
import qualified Data.ByteString.Char8 as BC
import           Control.Parallel.Strategies
import           Control.Concurrent.Async
import           Control.Monad.Par.IO
import           System.Random

getStdGens :: StdGen -> Int -> [StdGen]
getStdGens _ 0 = []
getStdGens gen n =
  let (g1,g2) = split gen in
  g1 : getStdGens g2 (n-1)

main :: IO ()
main = do
  wld <- randomWorld
  cg <- getStdGen
  let stdGens = getStdGens cg 1
      scene = makeScene 400 200 400 (13, 2, 3) (0, 0, 0) 20 0.1 wld  
  pics <- mapConcurrently (\x -> runReaderT (renderScene x) scene) stdGens
  let lp = fromIntegral $ length pics 
      pic = map (printVect . mO (/lp)) $ foldl1 (zipWith (+)) pics
  BC.writeFile "out.ppm" (BC.unlines $ header scene <> pic)
  --BC.putStrLn . printHeader $ scene
  --forM_ pic (BC.putStrLn . printVect)

{-# LANGUAGE OverloadedStrings #-}
module Scene where

import           Camera
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8      as BC
import           Hitable
import           HitableList
import           Ray
import           System.Random
import           Vect3
import           Control.Parallel.Strategies
import           Control.Monad.Par (spawn)
import           Control.Monad.Par.IO
import           Control.Monad.Trans
import           Control.Concurrent.Async

data Scene = MkScene {
    header :: [BC.ByteString],
    nx     :: Int,
    ny     :: Int,
    ns     :: Int,
    camera :: Camera,
    world  :: HitableList
}

makeScene :: Int -> Int -> Int -> Vect3 -> Vect3 ->
             Double -> Double -> HitableList -> Scene
makeScene nx ny ns lookfrom lookat fov aperture world =
  let header = [ "P3", BC.pack (show nx) <> " " <> BC.pack (show ny), "255" ]
      distToFocus = len (lookfrom - lookat)
      camera = makeCamera lookfrom lookat (0,1,0) fov (fI nx / fI ny) aperture distToFocus in
      MkScene header nx ny ns camera world
  where
    fI = fromIntegral

color :: Ray -> HitableList -> Int -> IO Vect3
color r@(MkRay origin direction) world depth =
  case hitL world r 0.001 9.9e999 of
       Nothing -> let unitDir = unit direction
                      t = 0.5 * getY unitDir + 1.0 in
                      return $ mO (*(1.0-t)) (1.0, 1.0, 1.0) + mO (*t) (0.5, 0.7, 1.0)
       Just rec -> do
         (attenuation, scattered, bool) <- scatter rec r
         if depth < 50 && bool
            then do
              col <- color scattered world (depth+1)
              return $ attenuation * col
         else return (0,0,0)

printHeader :: Scene -> BC.ByteString
printHeader (MkScene header _ _ _ _ _) = BC.unlines header

printVect :: Vect3 -> BC.ByteString
printVect v =
  let (x,y,z) = mO floor v in
      BC.pack (show x) <> " " <> BC.pack (show y) <> " " <> BC.pack (show z)


getSamples :: Int -> Int -> Int -> Int -> Camera -> HitableList ->
              Vect3 -> Int -> IO Vect3
getSamples i j nx ny camera world col a= do
  r1 <- liftIO $ randomRIO (0.0, 0.9999999999999999)
  r2 <- liftIO $ randomRIO (0.0, 0.9999999999999999)
  let u = (fromIntegral i + r1) / fromIntegral nx
      v = (fromIntegral j + r2) / fromIntegral ny
  r <- getRay camera u v
  c1 <- color r world 0
  return (col + c1)

renderCol :: (Int, Int) -> ReaderT Scene IO Vect3
renderCol (j, i) = do
  (MkScene header nx ny ns camera world) <- ask

  col <- liftIO $ foldM (getSamples i j nx ny camera world) (0, 0, 0) [0..(ns-1)]
  --col <- liftIO $ replicateConcurrently ns (getSamples i j nx ny camera world)
  return $ mO (\x -> (*) 255.99 $ sqrt $ x / fromIntegral ns) col

renderScene :: StdGen -> ReaderT Scene IO [Vect3]
renderScene g = do
  liftIO $ setStdGen g
  scene@(MkScene header nx ny ns camera world) <- ask
  let ni = ny `div` 2
      genCols x = runReaderT (renderCol x) scene
  pic <- liftIO $ forM [(j, i) | j <- [(ny-1),(ny-2)..0], i <- [0..(nx-1)]] genCols
  return pic

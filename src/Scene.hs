{-# LANGUAGE OverloadedStrings #-}
module Scene where

import           Camera
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8         as BC
import           Hitable
import           HitableList
import           Ray
import           System.Random
import           Vect3
import           Control.Parallel.Strategies

data Scene = MkScene {
    header :: [BC.ByteString],
    nx     :: Int,
    ny     :: Int,
    ns     :: Int,
    camera :: Camera,
    world  :: HitableList
}

makeScene
  :: Int
  -> Int
  -> Int
  -> Vect3
  -> Vect3
  -> Double
  -> Double
  -> HitableList
  -> Scene
makeScene nx ny ns lookfrom lookat fov aperture world =
  let
    header      = ["P3", BC.pack (show nx) <> " " <> BC.pack (show ny), "255"]
    distToFocus = len (lookfrom - lookat)
    camera      = makeCamera lookfrom
                             lookat
                             (0, 1, 0)
                             fov
                             (fI nx / fI ny)
                             aperture
                             distToFocus
  in
    MkScene header nx ny ns camera world
  where fI = fromIntegral

color :: Ray -> HitableList -> Int -> StdGen -> Vect3
color r@(MkRay origin direction) world depth stdgen =
  case hitL world r 0.001 9.9e999 of
    Nothing ->
      let unitDir = unit direction
          t       = 0.5 * getY unitDir + 1.0
      in  mO (* (1.0 - t)) (1.0, 1.0, 1.0) + mO (* t) (0.5, 0.7, 1.0)
    Just rec ->
      let (attenuation, scattered, bool) = scatter rec r stdgen
      in  if depth < 50 && bool
            then
              let ns  = snd $ split stdgen
                  col = color scattered world (depth + 1) ns
              in  attenuation * col
            else (0, 0, 0)

printHeader :: Scene -> BC.ByteString
printHeader (MkScene header _ _ _ _ _) = BC.unlines header

printVect :: Vect3 -> BC.ByteString
printVect v =
  let (x, y, z) = mO floor v
  in  BC.pack (show x) <> " " <> BC.pack (show y) <> " " <> BC.pack (show z)

renderCol
  :: Int
  -> Int
  -> Int
  -> Int
  -> Camera
  -> HitableList
  -> ((Double, Double), StdGen)
  -> Vect3
renderCol i j nx ny camera world ((r1, r2), stdGen) =
  let u        = (fromIntegral i + r1) / fromIntegral nx
      v        = (fromIntegral j + r2) / fromIntegral ny
      (s1, s2) = split stdGen
      r        = getRay camera u v s1
  in  color r world 0 s2

gSG :: StdGen -> Int -> [StdGen]
gSG _  0 = []
gSG sg n = let (s1, s2) = split sg in s1 : gSG s2 (n - 1)

getSamples :: (Int, Int) -> ReaderT Scene IO Vect3
getSamples (j, i) = do
  (MkScene header nx ny ns camera world) <- ask
  stdgen <- liftIO newStdGen
  let rands = take ns $ makeTups $ randomRs (0.0, 0.9999999999999999) stdgen
      ng    = snd $ split stdgen
      gens  = map mkStdGen $ take ns $ randomRs (0, maxBound :: Int) ng
      col = parMap rdeepseq (renderCol i j nx ny camera world) $ zip rands gens
  return $ mO (\x -> (*) 255.99 $ sqrt $ x / fromIntegral ns) $ sum col
 where
  makeTups :: [Double] -> [(Double, Double)]
  makeTups []          = []
  makeTups (x : y : z) = (x, y) : makeTups z

renderScene :: ReaderT Scene IO [Vect3]
renderScene = do
  scene@(MkScene _ nx ny _ _ _) <- ask
  let genCols x = runReaderT (getSamples x) scene
  liftIO $ forM
    [ (j, i) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)] ]
    genCols

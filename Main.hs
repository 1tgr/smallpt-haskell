{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Data.Ord
import Debug.Trace
import List
import Maybe
import Random

data Vec a = Vec a a a
             deriving Show

(|+|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |+| (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

(|-|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |-| (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

(|*|) :: Num a => Vec a -> a -> Vec a
(Vec x y z) |*| n = Vec (x * n) (y * n) (z * n)

vmult :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `vmult` (Vec x2 y2 z2) = Vec (x1 * x2) (y1 * y2) (z1 * z2)

norm :: Floating a => Vec a -> Vec a
norm v = let Vec x y z = v in v |*| (1 / sqrt ((x * x) + (y * y) + (z * z)))

dot :: Num a => Vec a -> Vec a -> a
(Vec x1 y1 z1) `dot` (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `cross` (Vec x2 y2 z2) = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
  
data Ray a = Ray (Vec a) (Vec a)

data Refl = DIFF
          | SPEC
          | REFR

data Sphere a = Sphere { radius :: a,
                         position :: Vec a,
                         emission :: Vec a,
                         colour :: Vec a,
                         refl :: Refl }

intersectSphere :: (Floating a, Ord a) => Sphere a -> Ray a -> a
intersectSphere s (Ray o d) = let op = (position s) |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                  eps = 1e-4
                                  b = op `dot` d
                                  det = b * b - (op `dot` op) + (radius s) * (radius s)
                              in if det < 0
                                   then 0
                                   else let det' = sqrt det
                                            t = b - det'
                                        in if t > eps
                                             then t
                                             else let t' = b + det'
                                                  in if t' > eps
                                                       then t'
                                                       else 0

spheres :: Fractional a => [Sphere a]
spheres = [Sphere { radius = 1e5,  position = Vec (1+1e5) 40.8 81.6,    emission = Vec 0 0 0,    colour = Vec 0.75 0.25 0.25,  refl = DIFF},--Left
           Sphere { radius = 1e5,  position = Vec (99-1e5) 40.8 81.6,   emission = Vec 0 0 0,    colour = Vec 0.25 0.25 0.75,  refl = DIFF},--Rght
           Sphere { radius = 1e5,  position = Vec 50 40.8 1e5,          emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Back
           Sphere { radius = 1e5,  position = Vec 50 40.8 (170-1e5),    emission = Vec 0 0 0,    colour = Vec 0 0 0,           refl = DIFF},--Frnt
           Sphere { radius = 1e5,  position = Vec 50 1e5 81.6,          emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Botm
           Sphere { radius = 1e5,  position = Vec 50 (81.6-1e5) 81.6,   emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Top
           Sphere { radius = 16.5, position = Vec 27 16.5 47,           emission = Vec 0 0 0,    colour = (Vec 1 1 1)|*|0.999, refl = SPEC},--Mirr
           Sphere { radius = 16.5, position = Vec 73 16.5 78,           emission = Vec 0 0 0,    colour = (Vec 1 1 1)|*|0.999, refl = REFR},--Glas
           Sphere { radius = 600,  position = Vec 50 (681.6-0.27) 81.6, emission = Vec 12 12 12, colour = Vec 0 0 0,           refl = DIFF}] --Lite

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0 = 0
        | x > 1 = 1
        | otherwise = x

toInt :: (Floating a, Ord a) => a -> a
toInt x = ((((clamp x) ** (1 / 2.2)) * 255) + 0.5)

intersectScene :: (Floating a, Ord a) => Ray a -> Maybe (Sphere a, a)
intersectScene r = listToMaybe
                 $ reverse
                 $ sortBy (comparing snd)
                 $ [(s, t) | s <- spheres, let t = intersectSphere s r, t /= 0]

radiance :: Ray a -> Int -> Vec a
radiance = undefined

main' :: (Enum a, Floating a, Ord a, Random a, Read a, RandomGen g) => Int -> State g [Vec a]
main' samp = let w = 1024
                 h = 768
                 floatsamp = fromInteger (toInteger samp) :: Floating f => f
                 campos = Vec 50 52 295.6
                 camdir = norm (Vec 0 (-0.042612) (-1))
                 cx = Vec (w*0.5135/h) 0 0
                 cy = (norm (cx `cross` camdir)) |*| 0.5135
                 zz x y = let zzz sx sy = let zzzz = do r1 <- (2 *) <$> State random
                                                        r2 <- (2 *) <$> State random
                                                        let dx | r1 < 1 = (sqrt r1) - 1
                                                               | otherwise = 1 - (sqrt (2 - r1))
                                                            dy | r2 < 1 = (sqrt r2) - 1
                                                               | otherwise = 1 - (sqrt (2-r2))
                                                            d = (cx |*| ( ( (sx + 0.5 + dx)/2 + x)/w - 0.5)) |+|
                                                                (cy |*| ( ( (sy + 0.5 + dy)/2 + y)/h - 0.5)) |+| camdir
                                                            ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                                        return ((radiance ray 0) |*| (1.0 / floatsamp))
                                          in do Vec rx ry rz <- foldl1 (|+|) <$> sequence (genericReplicate samp zzzz)
                                                return (Vec (clamp rx) (clamp ry) (clamp rz) |*| 0.25)
                          in foldl1 (|+|) <$> sequence [zzz sx sy | sy <- [0..1], sx <- [0..1]]
             in sequence [zz x y | y <- [0..(h-1)], x <- trace ("y = " ++ (show y)) [0..(w-1)]]

main :: IO ()
main = do let c :: [Vec Float]
              c = evalState (main' 8) (mkStdGen 0)
          print c
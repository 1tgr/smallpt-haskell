{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.State
import Control.Parallel.Strategies(parMap, rdeepseq)
import Data.Ord
import Debug.Trace
import List
import Maybe
import Random
import System

data Vec a = Vec a a a
             deriving Show

instance Functor Vec where
  fmap f (Vec x y z) = Vec (f x) (f y) (f z)

instance NFData a => NFData (Vec a) where
  rnf (Vec x y z) = rnf x `seq` rnf y `seq` rnf z

(|+|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |+| (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

(|-|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |-| (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

(|*|) :: Num a => Vec a -> a -> Vec a
v |*| n = fmap (* n) v

vmult :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `vmult` (Vec x2 y2 z2) = Vec (x1 * x2) (y1 * y2) (z1 * z2)

norm :: Floating a => Vec a -> Vec a
norm v = let Vec x y z = v in v |*| (1 / sqrt ((x * x) + (y * y) + (z * z)))

dot :: Num a => Vec a -> Vec a -> a
(Vec x1 y1 z1) `dot` (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `cross` (Vec x2 y2 z2) = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
  
data Ray a = Ray (Vec a) (Vec a)

data Refl = DIFF
          | SPEC
          | REFR

data Sphere a = Sphere { radius :: a,
                         position :: Vec a,
                         emission :: Vec a,
                         colour :: Vec a,
                         refl :: Refl }

intersectSphere :: (Floating a, Ord a) => Ray a -> Sphere a -> Maybe a
intersectSphere (Ray o d) s | det < 0 = Nothing
                            | t > eps = Just t
                            | t' > eps = Just t'
                            | otherwise = Nothing
                            where op = position s |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                  eps = 1e-4
                                  b = op `dot` d
                                  det = (b * b) - (op `dot` op) + (radius s * radius s)
                                  det' = sqrt det
                                  t = b - det'
                                  t' = b + det'

spheres :: Fractional a => [Sphere a]
spheres = [Sphere { radius = 1e5,  position = Vec (1+1e5) 40.8 81.6,    emission = Vec 0 0 0,    colour = Vec 0.75 0.25 0.25,  refl = DIFF},--Left
           Sphere { radius = 1e5,  position = Vec (99-1e5) 40.8 81.6,   emission = Vec 0 0 0,    colour = Vec 0.25 0.25 0.75,  refl = DIFF},--Rght
           Sphere { radius = 1e5,  position = Vec 50 40.8 1e5,          emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Back
           Sphere { radius = 1e5,  position = Vec 50 40.8 (170-1e5),    emission = Vec 0 0 0,    colour = Vec 0 0 0,           refl = DIFF},--Frnt
           Sphere { radius = 1e5,  position = Vec 50 1e5 81.6,          emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Botm
           Sphere { radius = 1e5,  position = Vec 50 (81.6-1e5) 81.6,   emission = Vec 0 0 0,    colour = Vec 0.75 0.75 0.75,  refl = DIFF},--Top
           Sphere { radius = 16.5, position = Vec 27 16.5 47,           emission = Vec 0 0 0,    colour = Vec 1 1 1 |*| 0.999, refl = SPEC},--Mirr
           Sphere { radius = 16.5, position = Vec 73 16.5 78,           emission = Vec 0 0 0,    colour = Vec 1 1 1 |*| 0.999, refl = REFR},--Glas
           Sphere { radius = 600,  position = Vec 50 (681.6-0.27) 81.6, emission = Vec 12 12 12, colour = Vec 0 0 0,           refl = DIFF}] --Lite

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0 = 0
        | x > 1 = 1
        | otherwise = x

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

maybeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinimumBy _ [] = Nothing
maybeMinimumBy f l = Just (minimumBy f l)

intersectScene :: (Floating a, Ord a) => Ray a -> Maybe (Sphere a, a)
intersectScene r = maybeMinimumBy (comparing snd) [(s, t) | (s, Just t) <- map ((,) <*> intersectSphere r) spheres]

radiance' :: (Floating a, Ord a, Random a, RandomGen g) => Ray a -> Int -> Sphere a -> a -> State g (Vec a)
radiance' r depth obj t | depth >= 5 = return (emission obj) --R.R.
                        | otherwise = do p' <- State (randomR (0, 1))
                                         if p' >= p
                                           then return (emission obj) --R.R.
                                           else let f = colour obj |*| (1.0 / p) in ((emission obj) |+|) . (f `vmult`) <$> reflect (refl obj)
                        where Ray raypos raydir = r
                              x = raypos |+| (raydir |*| t)
                              n = norm (x |-| position obj)
                              nl | (n `dot` raydir) < 0 = n
                                 | otherwise = n |*| (-1)
                              p = let Vec fx fy fz = colour obj in maximum [fx, fy, fz]
                              reflRay = Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))
                              reflect DIFF = let w = nl                                -- Ideal DIFFUSE reflection
                                                 Vec wx _ _ = w
                                                 u | abs wx > 0.1 = norm (Vec 0 1 0 `cross` w)
                                                   | otherwise = norm (Vec 1 0 0 `cross` w)
                                                 v = w `cross` u
                                             in do r1 <- State (randomR (0, 2 * pi))
                                                   r2 <- State (randomR (0, 1))
                                                   let r2s = sqrt r2
                                                       d = norm ((u |*| (cos r1 * r2s)) |+| 
                                                                 (v |*| (sin r1 * r2s)) |+| 
                                                                 (w |*| sqrt (1 - r2)))
                                                   radiance (Ray x d) (depth + 1)
                              reflect SPEC = radiance reflRay (depth + 1)             -- Ideal SPECULAR reflection
                              reflect REFR | cos2t < 0 = radiance reflRay (depth + 1) -- Total internal reflection
                                           | depth >= 2 = do pp' <- State (randomR (0, 1))
                                                             if pp' < pp
                                                               then (|*| rp) <$> radiance reflRay (depth + 1)
                                                               else (|*| tp) <$> radiance (Ray x tdir) (depth + 1)
                                           | otherwise = do re' <- (|*| re) <$> radiance reflRay (depth + 1)
                                                            tr' <- (|*| tr) <$> radiance (Ray x tdir) (depth + 1)
                                                            return (re' |+| tr')    -- Ideal dielectric REFRACTION
                                           where into = (n `dot` nl) > 0             -- Ray from outside going in?
                                                 nc = 1
                                                 nt = 1.5
                                                 nnt | into = nc / nt
                                                     | otherwise = nt / nc
                                                 ddn = raydir `dot` nl
                                                 cos2t = 1 - (nnt * nnt * (1 - (ddn * ddn)))
                                                 tdir = norm ((raydir |*| nnt) |-| (n |*| ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t))))
                                                 a = nt - nc
                                                 b = nt + nc
                                                 r0 = a * a / (b * b)
                                                 c | into = 1 + ddn
                                                   | otherwise = 1 - tdir `dot` n
                                                 re = r0 + ((1 - r0) * c * c * c * c * c)
                                                 tr = 1 - re
                                                 pp = 0.25 + (0.5 * re)
                                                 rp = re / p
                                                 tp = tr / (1 - pp)

radiance :: (Floating a, Ord a, Random a, RandomGen g) => Ray a -> Int -> State g (Vec a)
radiance r depth | Just (obj, t) <- intersectScene r = radiance' r depth obj t
                 | otherwise = return (Vec 0 0 0)

main' :: forall a. (Enum a, Floating a, NFData a, Ord a, Random a) => Int -> Int -> Int -> [[Vec a]]
main' w h samp = parMap rdeepseq (line . (h -)) [1..h]
                 where one_over_w = 1.0 / fromIntegral w :: a
                       one_over_h = 1.0 / fromIntegral h :: a
                       one_over_samp = 1.0 / fromIntegral samp ::a
                       campos = Vec 50 52 295.6
                       camdir = norm (Vec 0 (-0.042612) (-1))
                       cx = Vec (0.5135 * one_over_h / one_over_w) 0 0
                       cy = norm (cx `cross` camdir) |*| 0.5135
                       line y = let m = mapM (flip pixel y . subtract 1) [1..w]
                                    g = mkStdGen (y * y * y)
                                in evalState (trace ("Line " ++ show y) m) g
                       pixel x y = (|*| 0.25) . foldl1 (|+|) <$> sequence [subpixel x y sx sy | sy <- [0 :: Int, 1], sx <- [0 :: Int, 1]]
                       subpixel x y sx sy = fmap clamp . (|*| one_over_samp) . foldl1 (|+|) <$> replicateM samp (sample x y sx sy)
                       sample x y sx sy = do r1 <- State (randomR (0, 2))
                                             r2 <- State (randomR (0, 2))
                                             let dx | r1 < 1 = sqrt r1 - 1
                                                    | otherwise = 1 - sqrt (2 - r1)
                                                 dy | r2 < 1 = sqrt r2 - 1
                                                    | otherwise = 1 - sqrt (2 - r2)
                                                 d = (cx |*| ((((fromIntegral sx + 0.5 + dx) / 2 + fromIntegral x) * one_over_w) - 0.5)) |+|
                                                     (cy |*| ((((fromIntegral sy + 0.5 + dy) / 2 + fromIntegral y) * one_over_h) - 0.5)) |+| camdir
                                                 ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                             radiance ray 0

main :: IO ()
main = do args <- getArgs
          let w = 1024
              h = 768
              samp | s:_ <- args = read s `div` 4
                   | otherwise = 1
              showPixel (Vec r g b) = show (toInt r :: Int) ++ " " ++ show (toInt g :: Int) ++ " " ++ show (toInt b :: Int) ++ " "
          putStrLn ("P3\n" ++ show w ++ " " ++ show h ++ "\n255\n")
          mapM_ (putStrLn . showPixel) (concat (main' w h samp) :: [Vec Double])
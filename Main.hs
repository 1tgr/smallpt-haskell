{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Control.Parallel.Strategies(parMap, rwhnf)
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

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate ((((clamp x) ** (1 / 2.2)) * 255) + 0.5)

intersectScene :: (Floating a, Ord a) => Ray a -> Maybe (Sphere a, a)
intersectScene r = listToMaybe
                 $ sortBy (comparing snd)
                 $ [(s, t) | s <- spheres, let t = intersectSphere s r, t /= 0]

radiance :: (Floating a, Ord a, Random a, RandomGen g) => Ray a -> Int -> State g (Vec a)
radiance r depth = case intersectScene r of
                     Nothing -> return (Vec 0 0 0)
                     Just (obj, t) -> radiance' obj t
  where radiance' obj t = do let Ray rayo rayd = r
                                 x = rayo |+| (rayd |*| t)
                                 n = norm (x |-| (position obj))
                                 nl | (n `dot` rayd) < 0 = n
                                    | otherwise = n |*| (-1)
                                 Vec fx fy fz = colour obj
                                 p | fx > fy && fx > fz = fx
                                   | fy > fz = fy
                                   | otherwise = fz -- max refl
                             p' <- if depth >= 5
                                     then return p
                                     else State random
                             if p' >= p
                               then return (emission obj) --R.R.
                               else let f = (colour obj) |*| (1.0 / p)
                                    in case refl obj of 
                                      DIFF -> -- Ideal DIFFUSE reflection
                                              do r1 <- (2 * pi *) <$> State random
                                                 r2 <- State random
                                                 let r2s = sqrt r2
                                                     Vec wx _ _ = nl
                                                     u | (abs wx) > 0.1 = Vec 0 1 0
                                                       | otherwise = norm ((Vec 1 0 0) `cross` nl)
                                                     v = nl `cross` u
                                                     d = norm ((u |*| (cos r1) |*| r2s) |+| (v |*| (sin r1) |*| r2s) |+| (nl |*| (sqrt (1 - r2))))
                                                 ((emission obj) |+|) . (f `vmult`) <$> radiance (Ray x d) (depth + 1)
                                      SPEC -> -- Ideal SPECULAR reflection
                                              ((emission obj) |+|) . (f `vmult`) <$> radiance (Ray x (rayd |-| (n |*| (2 * (n `dot` rayd))))) (depth + 1)
                                      REFR -> -- Ideal dielectric REFRACTION
                                              let reflRay = Ray x (rayd |-| (n |*| (2 * (n `dot` rayd))))
                                                  into = (n `dot` nl) > 0                -- Ray from outside going in?
                                                  nc = 1
                                                  nt = 1.5
                                                  nnt | into = nc / nt
                                                      | otherwise = nt / nc
                                                  ddn = rayd `dot` nl
                                                  cos2t = 1 - nnt * nnt * (1 - ddn * ddn)
                                              in if cos2t < 0    -- Total internal reflection
                                                   then ((emission obj) |+|) . (f `vmult`) <$> radiance reflRay (depth + 1)
                                                   else let tdir = norm ((rayd |*| nnt) |-| (n |*| ((if into then 1 else (-1)) * (ddn * nnt + (sqrt cos2t)))))
                                                            a = nt - nc
                                                            b = nt + nc
                                                            r0 = a * a / (b * b)
                                                            c = 1 - (if into then (-ddn) else tdir `dot` n)
                                                            re = r0 + (1 - r0) * c * c * c * c * c
                                                            tr = 1 - re
                                                            pp = 0.25 + 0.5 * re
                                                            rp = re / p
                                                            tp = tr / (1 - p)
                                                            r' | depth >= 2 = do pp' <- State random
                                                                                 if pp' < pp
                                                                                   then (|*| rp) <$> radiance reflRay (depth + 1)
                                                                                   else (|*| tp) <$> radiance (Ray x tdir) (depth + 1)
                                                               | otherwise = do re' <- (|*| re) <$> radiance reflRay (depth + 1)
                                                                                tr' <- (|*| tr) <$> radiance (Ray x tdir) (depth + 1)
                                                                                return (re' |+| tr')
                                                        in ((emission obj) |+|) . (f `vmult`) <$> r'

main' :: (Enum a, Floating a, Ord a, Random a) => Int -> Int -> Int -> [[Vec a]]
main' w h samp = parMap rwhnf (line . (h -)) [1..h]
                 where floatw = fromInteger (toInteger w) :: Floating f => f
                       floath = fromInteger (toInteger h) :: Floating f => f
                       floatsamp = fromInteger (toInteger samp) :: Floating f => f
                       campos = Vec 50 52 295.6
                       camdir = norm (Vec 0 (-0.042612) (-1))
                       cx = Vec (floatw * 0.5135 / floath) 0 0
                       cy = (norm (cx `cross` camdir)) |*| 0.5135
                       line y = let m = sequence (map (pixel . subtract 1) [1..w])
                                    g = mkStdGen (y * y * y)
                                in evalState m (trace ("Line " ++ (show y)) g)
                                where floaty = fromInteger (toInteger y) :: Floating f => f
                                      pixel x = foldl1 (|+|) <$> sequence [subpixel sx sy | sy <- [0, 1], sx <- [0, 1]]
                                                where floatx = fromInteger (toInteger x) :: Floating f => f
                                                      subpixel sx sy = do Vec rx ry rz <- foldl1 (|+|) <$> sequence (replicate samp sample)
                                                                          return (Vec (clamp rx) (clamp ry) (clamp rz) |*| 0.25)
                                                                       where sample = do r1 <- (2 *) <$> State random
                                                                                         r2 <- (2 *) <$> State random
                                                                                         let dx | r1 < 1 = (sqrt r1) - 1
                                                                                                | otherwise = 1 - (sqrt (2 - r1))
                                                                                             dy | r2 < 1 = (sqrt r2) - 1
                                                                                                | otherwise = 1 - (sqrt (2-r2))
                                                                                             d = (cx |*| (((sx + 0.5 + dx)/2 + floatx)/floatw - 0.5)) |+|
                                                                                                 (cy |*| (((sy + 0.5 + dy)/2 + floaty)/floath - 0.5)) |+| camdir
                                                                                             ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                                                                         (|*| (1.0 / floatsamp)) <$> radiance ray 0

main :: IO ()
main = do let w = 320
              h = 240
              showPixel (Vec r g b) = (show (toInt r :: Int)) ++ " " ++ (show (toInt g :: Int)) ++ " " ++ (show (toInt b :: Int)) ++ " "
          putStrLn ("P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n255\n")
          mapM_ (putStrLn . showPixel) (concat (main' w h 8) :: [Vec Float])
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

data Vec a = Vec a a a
             deriving Show

instance NFData a => NFData (Vec a) where
  rnf (Vec x y z) = rnf x `seq` rnf y `seq` rnf z

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

intersectSphere :: (Floating a, Ord a) => Sphere a -> Ray a -> Maybe a
intersectSphere s (Ray o d) = let op = (position s) |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                  eps = 1e-4
                                  b = op `dot` d
                                  det = (b * b) - (op `dot` op) + ((radius s) * (radius s))
                              in if det < 0
                                   then Nothing
                                   else let det' = sqrt det
                                            t = b - det'
                                        in if t > eps
                                             then Just t
                                             else let t' = b + det'
                                                  in if t' > eps
                                                       then Just t'
                                                       else Nothing

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

maybeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinimumBy f [] = Nothing
maybeMinimumBy f l = Just (minimumBy f l)

intersectScene :: (Floating a, Ord a) => Ray a -> Maybe (Sphere a, a)
intersectScene r = maybeMinimumBy (comparing snd)
                 $ [(s, t) | (s, Just t) <- [(s, intersectSphere s r) | s <- spheres]]

radiance :: (Floating a, Ord a, Random a, RandomGen g) => Ray a -> Int -> State g (Vec a)
radiance r depth = case intersectScene r of
                     Nothing -> return (Vec 0 0 0)
                     Just (obj, t) -> radiance' obj t
  where radiance' obj t = do let Ray raypos raydir = r
                                 x = raypos |+| (raydir |*| t)
                                 n = norm (x |-| (position obj))
                                 nl | (n `dot` raydir) < 0 = n
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
                                                     u | (abs wx) > 0.1 = norm ((Vec 0 1 0) `cross` nl)
                                                       | otherwise = norm ((Vec 1 0 0) `cross` nl)
                                                     v = nl `cross` u
                                                     d = norm ((u |*| ((cos r1) * r2s)) |+| 
                                                               (v |*| ((sin r1) * r2s)) |+| 
                                                               (nl |*| (sqrt (1 - r2))))
                                                 ((emission obj) |+|) . (f `vmult`) <$> radiance (Ray x d) (depth + 1)
                                      SPEC -> -- Ideal SPECULAR reflection
                                              ((emission obj) |+|) . (f `vmult`) <$> radiance (Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))) (depth + 1)
                                      REFR -> -- Ideal dielectric REFRACTION
                                              let reflRay = Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))
                                                  into = (n `dot` nl) > 0                -- Ray from outside going in?
                                                  nc = 1
                                                  nt = 1.5
                                                  nnt | into = nc / nt
                                                      | otherwise = nt / nc
                                                  ddn = raydir `dot` nl
                                                  cos2t = 1 - (nnt * nnt * (1 - (ddn * ddn)))
                                              in if cos2t < 0    -- Total internal reflection
                                                   then ((emission obj) |+|) . (f `vmult`) <$> radiance reflRay (depth + 1)
                                                   else let tdir = norm ((raydir |*| nnt) |-| (n |*| ((if into then 1 else (-1)) * (ddn * nnt + (sqrt cos2t)))))
                                                            a = nt - nc
                                                            b = nt + nc
                                                            r0 = a * a / (b * b)
                                                            c = 1 - (if into then (-ddn) else tdir `dot` n)
                                                            re = r0 + ((1 - r0) * c * c * c * c * c)
                                                            tr = 1 - re
                                                            pp = 0.25 + (0.5 * re)
                                                            rp = re / p
                                                            tp = tr / (1 - pp)
                                                            r' | depth >= 2 = do pp' <- State random
                                                                                 if pp' < pp
                                                                                   then (|*| rp) <$> radiance reflRay (depth + 1)
                                                                                   else (|*| tp) <$> radiance (Ray x tdir) (depth + 1)
                                                               | otherwise = do re' <- (|*| re) <$> radiance reflRay (depth + 1)
                                                                                tr' <- (|*| tr) <$> radiance (Ray x tdir) (depth + 1)
                                                                                return (re' |+| tr')
                                                        in ((emission obj) |+|) . (f `vmult`) <$> r'

main' :: forall a. (Enum a, Floating a, NFData a, Ord a, Random a) => Int -> Int -> Int -> [[Vec a]]
main' w h samp = parMap rdeepseq (line . (h -)) [1..h]
                 where one_over_w = 1.0 / (fromIntegral w) :: a
                       one_over_h = 1.0 / (fromIntegral h) :: a
                       one_over_samp = 1.0 / (fromIntegral samp) ::a
                       campos = Vec 50 52 295.6
                       camdir = norm (Vec 0 (-0.042612) (-1))
                       cx = Vec (0.5135 * one_over_h / one_over_w) 0 0
                       cy = (norm (cx `cross` camdir)) |*| 0.5135
                       line y = let m = sequence (map ((flip pixel y) . subtract 1) [1..w])
                                    g = mkStdGen (y * y * y)
                                in evalState (trace ("Line " ++ (show y)) m) g
                       pixel x y = foldl1 (|+|) <$> sequence [subpixel x y sx sy | sy <- [0, 1], sx <- [0, 1]]
                       subpixel x y sx sy = do Vec rx ry rz <- foldl1 (|+|) <$> sequence (replicate samp (sample x y sx sy))
                                               return (Vec (clamp rx) (clamp ry) (clamp rz) |*| 0.25)
                       sample x y sx sy = do r1 <- (2 *) <$> State random
                                             r2 <- (2 *) <$> State random
                                             let dx | r1 < 1 = (sqrt r1) - 1
                                                    | otherwise = 1 - (sqrt (2 - r1))
                                                 dy | r2 < 1 = (sqrt r2) - 1
                                                    | otherwise = 1 - (sqrt (2 - r2))
                                                 floatx = fromIntegral x :: a
                                                 floaty = fromIntegral y :: a
                                                 d = (cx |*| (((sx + 0.5 + dx) / 2 + floatx) * one_over_w - 0.5)) |+|
                                                     (cy |*| (((sy + 0.5 + dy) / 2 + floaty) * one_over_h - 0.5)) |+| camdir
                                                 ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                             (|*| one_over_samp) <$> radiance ray 0

main :: IO ()
main = do let w = 1024
              h = 768
              showPixel (Vec r g b) = (show (toInt r :: Int)) ++ " " ++ (show (toInt g :: Int)) ++ " " ++ (show (toInt b :: Int)) ++ " "
          putStrLn ("P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n255\n")
          mapM_ (putStrLn . showPixel) (concat (main' w h 1) :: [Vec Float])
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Tim.Smallpt.Render(
  Context(..),
  Refl(..),
  Sphere(..),
  Vec(..),
  Work(..),
  (|*|), 
  (|+|), 
  (|-|),
  clamp, 
  cross,
  dot, 
  line,
  makeWork,
  norm, 
  vmult) where

import Control.Applicative
import Control.Monad.State
import Data.Data
import Data.Ord
import Data.List
import Data.Typeable
import Random

data Vec a = Vec a a a
             deriving (Data, Typeable)

instance Functor Vec where
  fmap f (Vec x y z) = Vec (f x) (f y) (f z)

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
          deriving (Data, Typeable)

data Sphere a = Sphere { radius :: a,
                         position :: Vec a,
                         emission :: Vec a,
                         colour :: Vec a,
                         refl :: Refl }
                deriving (Data, Typeable)

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

maybeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinimumBy _ [] = Nothing
maybeMinimumBy f l = Just (minimumBy f l)

intersectScene :: (Floating a, Ord a) => [Sphere a] -> Ray a -> Maybe (Sphere a, a)
intersectScene scene r = maybeMinimumBy (comparing snd) [(s, t) | (s, Just t) <- map ((,) <*> intersectSphere r) scene]

radiance' :: (Floating a, Ord a, Random a, RandomGen g) => [Sphere a] -> Ray a -> Int -> Sphere a -> a -> State g (Vec a)
radiance' scene r depth obj t | depth >= 5 = return (emission obj) --R.R.
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
                                                         radiance scene (Ray x d) (depth + 1)
                                    reflect SPEC = radiance scene reflRay (depth + 1)             -- Ideal SPECULAR reflection
                                    reflect REFR | cos2t < 0 = radiance scene reflRay (depth + 1) -- Total internal reflection
                                                 | depth >= 2 = do pp' <- State (randomR (0, 1))
                                                                   if pp' < pp
                                                                     then (|*| rp) <$> radiance scene reflRay (depth + 1)
                                                                     else (|*| tp) <$> radiance scene (Ray x tdir) (depth + 1)
                                                 | otherwise = do re' <- (|*| re) <$> radiance scene reflRay (depth + 1)
                                                                  tr' <- (|*| tr) <$> radiance scene (Ray x tdir) (depth + 1)
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

radiance :: (Floating a, Ord a, Random a, RandomGen g) => [Sphere a] -> Ray a -> Int -> State g (Vec a)
radiance scene r depth | Just (obj, t) <- intersectScene scene r = radiance' scene r depth obj t
                       | otherwise = return (Vec 0 0 0)

data Context a = Context { ctxw :: Int,
                           ctxh :: Int,
                           ctxsamp :: Int,
                           ctxcx :: Vec a,
                           ctxcy :: Vec a,
                           ctxcamdir :: Vec a,
                           ctxcampos :: Vec a,
                           ctxscene :: [Sphere a] }
                 deriving (Data, Typeable)

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0 = 0
        | x > 1 = 1
        | otherwise = x

line :: (Floating a, Ord a, Random a) => Context a -> Int -> [Vec a]
line context y = evalState (mapM (pixel . subtract 1) [1..w]) (mkStdGen (y * y * y))
                 where Context { ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, ctxcamdir = camdir, ctxcampos = campos, ctxscene = scene } = context
                       pixel x = (|*| 0.25) . foldl1 (|+|) <$> sequence [subpixel x sx sy | sy <- [0 :: Int, 1], sx <- [0 :: Int, 1]]
                       subpixel x sx sy = fmap clamp . (|*| (1 / fromIntegral samp)) . foldl1 (|+|) <$> replicateM samp (sample x sx sy)
                       sample x sx sy = do r1 <- State (randomR (0, 4))
                                           r2 <- State (randomR (0, 4))
                                           let dx | r1 < 2 = sqrt r1 - 2
                                                  | otherwise = 2 - sqrt (4 - r1)
                                               dy | r2 < 2 = sqrt r2 - 2
                                                  | otherwise = 2 - sqrt (4 - r2)
                                               d = (cx |*| ((((fromIntegral sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w) - 0.5)) |+|
                                                   (cy |*| ((((fromIntegral sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h) - 0.5)) |+| camdir
                                               ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                           radiance scene ray 0

data Work a = RenderLine a Int
              deriving (Data, Typeable)

makeWork :: Floating a => Int -> Int -> Int -> [Sphere a] -> [Work (Context a)]
makeWork w h samp scene = map (RenderLine context . (h -)) [1..h]
                          where context = Context { ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, ctxcampos = Vec 50 52 295.6, ctxcamdir = camdir, ctxscene = scene }
                                camdir = norm (Vec 0 (-0.042612) (-1))
                                cx = Vec (0.5135 * fromIntegral w / fromIntegral h) 0 0
                                cy = norm (cx `cross` camdir) |*| 0.5135

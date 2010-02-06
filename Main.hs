{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Data.Ord
import Graphics.GD
import List
import Maybe
import Random
import System
import System.IO
import System.Process
import System.Posix.Process

data Vec a = Vec a a a
             deriving (Read, Show)

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

data Context a = Context { ctxw :: Int,
                           ctxh :: Int,
                           ctxsamp :: Int,
                           ctxcx :: Vec a,
                           ctxcy :: Vec a,
                           ctxcamdir :: Vec a,
                           ctxcampos :: Vec a }
                 deriving (Read, Show)

line :: (Floating a, Ord a, Random a) => Context a -> Int -> [Vec a]
line context y = evalState (mapM (pixel . subtract 1) [1..w]) (mkStdGen (y * y * y))
                 where Context { ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, ctxcamdir = camdir, ctxcampos = campos } = context
                       pixel x = (|*| 0.25) . foldl1 (|+|) <$> sequence [subpixel x sx sy | sy <- [0 :: Int, 1], sx <- [0 :: Int, 1]]
                       subpixel x sx sy = fmap clamp . (|*| (1 / fromIntegral samp)) . foldl1 (|+|) <$> replicateM samp (sample x sx sy)
                       sample x sx sy = do r1 <- State (randomR (0, 2))
                                           r2 <- State (randomR (0, 2))
                                           let dx | r1 < 1 = sqrt r1 - 1
                                                  | otherwise = 1 - sqrt (2 - r1)
                                               dy | r2 < 1 = sqrt r2 - 1
                                                  | otherwise = 1 - sqrt (2 - r2)
                                               d = (cx |*| ((((fromIntegral sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w) - 0.5)) |+|
                                                   (cy |*| ((((fromIntegral sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h) - 0.5)) |+| camdir
                                               ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                           radiance ray 0

collectBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
collectBy key value = map (\g -> (key (head g), map value g))
                    . groupBy (\a b -> key a == key b)
                    . sortBy (comparing key)

submitWork :: forall a b. (Show a, Read b) => [a] -> IO [b]
submitWork l = do v <- newEmptyMVar
                  threads <- mapM (uncurry (invoke v)) tasks
                  results <- mapM (const (takeMVar v)) threads
                  case [message | Right message <- results] of
                    message:_ -> fail message
                    [] -> return
                        $ map snd
                        $ sortBy (comparing fst)
                          [result | Left results' <- results, result <- results']

               where workers = replicate 8 ("/Users/Tim/Git/smallpt/bin/smallpt", ["-worker"])
               
                     tasks :: [((Int, (FilePath, [String])), [(Int, a)])]
                     tasks = collectBy fst snd
                           $ zip (cycle (zip [1..] workers)) 
                           $ zip [1..]
                             l

                     invoke :: MVar (Either [(Int, b)] String) -> (Int, (FilePath, [String])) -> [(Int, a)] -> IO ThreadId
                     invoke v (i, (p, a)) w = forkIO (do putStr (('>' : show i) ++ " ")
                                                         hFlush stdout
                                                         (exitCode, out, err) <- readProcessWithExitCode p a (show (map snd w))
                                                         putStr (('<' : show i) ++ " ")
                                                         hFlush stdout
                                                         case exitCode of
                                                           ExitSuccess -> do putStr err
                                                                             putMVar v (Left (zip ids (read out)))
                                                           ExitFailure _ -> putMVar v (Right err))
                                              where ids = map fst w

data Work a = RenderLine a Int
              deriving (Read, Show)

main' :: forall a. (Enum a, Floating a, Ord a, Random a, Read a) => Int -> Int -> Int -> IO [[Vec a]]
main' w h samp = submitWork (map (RenderLine context . (h -)) [1..h]) :: IO [[Vec a]]
                 where context :: Context a
                       context = Context { ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, ctxcampos = Vec 50 52 295.6, ctxcamdir = camdir }
                       camdir = norm (Vec 0 (-0.042612) (-1))
                       cx = Vec (0.5135 * fromIntegral w / fromIntegral h) 0 0
                       cy = norm (cx `cross` camdir) |*| 0.5135

work :: (Read a, Show b) => IO () -> (a -> b) -> IO ()
work coordinator worker = do pid <- getProcessID
                             args <- getArgs
                             case args of
                               ["-worker"] -> do inputs <- read <$> getContents
                                                 hPutStrLn stderr (show pid ++ " Hello from the worker - got " ++ show (length inputs) ++ " inputs")
                                                 print (map worker inputs)
                               _ -> do hPutStrLn stderr (show pid ++ " Hello from the coordinator")
                                       coordinator

main :: IO ()
main = work coordinator worker
       where coordinator = do args <- getArgs
                              let w = 1024
                                  h = 768
                                  samp | s:_ <- args = read s `div` 4
                                       | otherwise = 1
                              image <- newImage (w, h)
                              colours <- main' w h samp :: IO [[Vec Double]]
                              let setOnePixel y x v = let Vec r g b = fmap toInt v in setPixel (x, y) (rgb r g b) image
                                  setLinePixels (l, y) = zipWithM_ (setOnePixel y) [1..] l
                              mapM_ setLinePixels (zip colours [1..])
                              savePngFile "image.png" image
             worker (RenderLine context y) = line context y :: [Vec Double]
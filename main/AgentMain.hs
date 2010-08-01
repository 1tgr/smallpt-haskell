{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Data.Ord
import Graphics.GD
import List
import System
import System.Console.GetOpt
import System.IO
import System.IO.Error
import Tim.Smallpt.Distribution
import Tim.Smallpt.Render

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

data Options = Options { optRunWorker :: Bool,
                         optWidth :: Int,
                         optHeight :: Int,
                         optSamples :: Int,
                         optOutput :: String }

options :: [OptDescr (Options -> IO Options)]
options = [Option "r" ["worker"]  (NoArg (\opts -> return (opts { optRunWorker = True })))                   "act as a worker process",
           Option "w" ["width"]   (ReqArg (\s opts -> (\i -> opts { optWidth = i }) <$> readIO s) "WIDTH")   "image width",
           Option "h" ["height"]  (ReqArg (\s opts -> (\i -> opts { optHeight = i }) <$> readIO s) "HEIGHT") "image height",
           Option "s" ["samples"] (ReqArg (\s opts -> (\i -> opts { optSamples = i }) <$> readIO s) "SAMP")  "number of samples per pixel",
           Option "o" ["output"]  (ReqArg (\s opts -> return (opts { optOutput = s })) "FILE")               "image file name"]

defaultOptions :: Options
defaultOptions = Options { optRunWorker = False,
                           optWidth = 1024,
                           optHeight = 768,
                           optSamples = 4,
                           optOutput = "image.png" }

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

makeImage :: (Floating a, RealFrac a) => Int -> Int -> [[Vec a]] -> IO Image
makeImage w h colours = do image <- newImage (w, h)
                           let setOnePixel y x v = let Vec r g b = fmap toInt v in setPixel (x, y) (rgb r g b) image
                               setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l
                           mapM_ setLinePixels (zip colours [0..])
                           return image

main :: IO ()
main = catch (do args <- getOpt Permute options <$> getArgs
                 case args of
                   (o, workers, []) -> do let coord = coordinator (\(RenderLine context y) -> line context y :: [Vec Double])
                                          opts <- foldl (>>=) (return defaultOptions) o `catch` const (usage [])
                                          case opts of
                                            Options { optRunWorker = True } -> runWorker coord
                                            Options { optWidth = w, optHeight = h, optSamples = samples, optOutput = output } | _:_ <- workers ->
                                              submitWork coord workers (makeWork w h samples spheres)
                                                >>= makeImage w h
                                                >>= savePngFile output
                                            _ -> usage ["Need at least one worker"]
                   (_, _, errs) -> usage errs)
             (putStrLn . ioeGetErrorString)
       where usage errs = ioError (userError (concat (intersperse "\n" (errs ++ [usageInfo "Usage: smallpt [OPTION...] workers..." options]))))
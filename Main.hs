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
                   (o, workers, []) -> do opts <- foldl (>>=) (return defaultOptions) o `catch` const (usage [])
                                          case opts of
                                            Options { optRunWorker = True } -> let worker (RenderLine context y) = line context y :: [Vec Double] in runWorker worker
                                            Options { optWidth = w, optHeight = h, optSamples = samples, optOutput = output } ->
                                              if workers == []
                                                then usage ["Need at least one worker"]
                                                else do let work :: [Work (Context Double)]
                                                            work = makeWork w h samples
                                                        colours <- submitWork workers work :: IO [[Vec Double]]
                                                        savePngFile output =<< makeImage w h colours
                   (_, _, errs) -> usage errs)
             (putStrLn . ioeGetErrorString)
       where usage errs = ioError (userError (concat (intersperse "\n" (errs ++ [usageInfo "Usage: smallpt [OPTION...] workers..." options]))))
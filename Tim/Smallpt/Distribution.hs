{-# LANGUAGE ScopedTypeVariables #-}
module Tim.Smallpt.Distribution (runWorker, submitWork) where

import Control.Concurrent
import Data.Ord
import Data.List
import System
import System.IO
import System.Process

collectBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
collectBy key value = map (\g -> (key (head g), map value g))
                    . groupBy (\a b -> key a == key b)
                    . sortBy (comparing key)

submitWork :: forall a b. (Show a, Read b) => [String] -> [a] -> IO [b]
submitWork workers l = do v <- newEmptyMVar
                          threads <- mapM (uncurry (invoke v)) tasks
                          results <- mapM (const (takeMVar v)) threads
                          case [message | Right message <- results] of
                            message:_ -> fail message
                            [] -> return
                                $ map snd
                                $ sortBy (comparing fst)
                                  [result | Left results' <- results, result <- results']

                       where tasks :: [((Int, String), [(Int, a)])]
                             tasks = collectBy fst snd
                                   $ zip (cycle (zip [1..] workers))
                                   $ zip [1..]
                                     l

                             invoke :: MVar (Either [(Int, b)] String) -> (Int, String) -> [(Int, a)] -> IO ThreadId
                             invoke v (i, worker) w = forkIO (do putStr (('>' : show i) ++ " ")
                                                                 hFlush stdout
                                                                 (exitCode, out, err) <- readProcessWithExitCode p a (show (map snd w))
                                                                 putStr (('<' : show i) ++ " ")
                                                                 hFlush stdout
                                                                 case exitCode of
                                                                   ExitSuccess -> do putStr err
                                                                                     putMVar v (Left (zip ids (read out)))
                                                                   ExitFailure _ -> putMVar v (Right err))
                                                      where ids = map fst w
                                                            p:a = words worker

runWorker :: (Read a, Show b) => (a -> b) -> IO ()
runWorker worker = interact (show . map worker . read)

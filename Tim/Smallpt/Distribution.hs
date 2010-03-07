{-# LANGUAGE ScopedTypeVariables #-}
module Tim.Smallpt.Distribution (Coordinator(..), coordinator) where

import Control.Concurrent
import Data.Ord
import Data.List
import System
import System.IO
import System.Process

invoke :: (Show a, Read b) => MVar (Either [(Int, b)] String) -> (Int, String) -> [(Int, a)] -> IO ThreadId
invoke results (i, worker) work = forkIO (do putStr (('>' : show i) ++ " ")
                                             hFlush stdout
                                             (exitCode, out, err) <- readProcessWithExitCode p a (show (map snd work))
                                             putStr (('<' : show i) ++ " ")
                                             hFlush stdout
                                             case exitCode of
                                               ExitSuccess -> do putStr err
                                                                 putMVar results (Left (zip ids (read out)))
                                               ExitFailure _ -> putMVar results (Right err))
                         where ids = map fst work
                               p:a = words worker

collectBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
collectBy key value = map (\g -> (key (head g), map value g))
                    . groupBy (\a b -> key a == key b)
                    . sortBy (comparing key)

submitWork' :: forall a b. (Show a, Read b) => [String] -> [a] -> IO [b]
submitWork' workers l = do v <- newEmptyMVar
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

data (Show a, Read b) => Coordinator a b = Coordinator { submitWork :: [String] -> [a] -> IO [b],
                                                         runWorker :: IO () }

coordinator :: (Read a, Show a, Read b, Show b) => (a -> b) -> (Coordinator a b)
coordinator worker = Coordinator { submitWork = submitWork',
                                   runWorker = interact (show . map worker . read) }
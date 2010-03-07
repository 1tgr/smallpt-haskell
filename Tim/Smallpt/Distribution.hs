{-# LANGUAGE ScopedTypeVariables #-}
module Tim.Smallpt.Distribution (Coordinator(..), coordinator) where

import Control.Applicative
import Control.Concurrent
import Data.Ord
import Data.List
import System
import System.IO
import System.Process

invoke :: (Show a, Read b) => (Int, String) -> [(Int, a)] -> IO [(Int, b)]
invoke (num, worker) work = do putStr (('>' : show num) ++ " ")
                               hFlush stdout
                               (exitCode, out, err) <- readProcessWithExitCode p a (show (map snd work))
                               putStr (('<' : show num) ++ " ")
                               hFlush stdout
                               case exitCode of
                                 ExitSuccess -> do putStr err
                                                   return (zip ids (read out))
                                 ExitFailure _ -> fail err
                           where ids = map fst work
                                 p:a = words worker

catching :: IO a -> IO (Either a IOError)
catching m = (Left <$> m) `catch` (return . Right)

collectBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
collectBy key value = map (\g -> (key (head g), map value g))
                    . groupBy (\a b -> key a == key b)
                    . sortBy (comparing key)

makeTasks :: Ord a => [a] -> [b] -> [((Int, a), [(Int, b)])]
makeTasks workers = collectBy fst snd
                  . zip (cycle (zip [1..] workers))
                  . zip [1..]

submitWork' :: forall a b. (Show a, Read b) => [String] -> [a] -> IO [b]
submitWork' workers work = do v <- newEmptyMVar
                              threads <- mapM (forkIO . (putMVar v =<<) . catching . uncurry invoke) (makeTasks workers work)
                              results <- mapM (const (takeMVar v)) threads
                              case [ioe | Right ioe <- results] of
                                ioe:_ -> ioError ioe 
                                [] -> return
                                    $ map snd
                                    $ sortBy (comparing fst)
                                      [result | Left results' <- results, result <- results']

data (Show a, Read b) => Coordinator a b = Coordinator { submitWork :: [String] -> [a] -> IO [b],
                                                         runWorker :: IO () }

coordinator :: (Read a, Show a, Read b, Show b) => (a -> b) -> Coordinator a b
coordinator worker = Coordinator { submitWork = submitWork',
                                   runWorker = interact (show . map worker . read) }
module Tim.Smallpt.Distribution (Coordinator(..), coordinator) where

import Control.Concurrent
import Control.Exception (evaluate, try)
import Control.Monad
import Data.Ord
import Data.List
import System
import System.IO
import System.Process

invoke :: (Show a, Read b) => (Int, String) -> [(Int, a)] -> IO [(Int, b)]
invoke (num, worker) work = do putStr (('>' : show num) ++ "@" ++ show (length work) ++ " ")
                               hFlush stdout

                               (Just inh, Just outh, Just errh, pid) <-
                                   createProcess (shell worker){ std_in  = CreatePipe,
                                                                 std_out = CreatePipe,
                                                                 std_err = CreatePipe }

                               outMVar <- newEmptyMVar

                               out <- hGetContents outh
                               _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

                               err <- hGetContents errh
                               _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

                               hPrint inh (map snd work)
                               hFlush inh
                               hClose inh

                               takeMVar outMVar
                               takeMVar outMVar
                               hClose outh

                               exitCode <- waitForProcess pid
                               
                               putStr (('<' : show num) ++ " ")
                               hFlush stdout
                               case exitCode of
                                 ExitSuccess -> do putStr err
                                                   return (zip ids (read out))
                                 ExitFailure _ -> fail err
                           where ids = map fst work

makeTasks :: Int -> [a] -> [[a]]
makeTasks n tasks = case splitAt n tasks of
                      ([], _) -> []
                      (firstN, rest) -> firstN : makeTasks n rest

workerLoop :: (Show a, Read b) => MVar [[(Int, a)]] -> MVar (Either IOError [(Int, b)]) -> (Int, String) -> IO ()
workerLoop tasksMVar resultsMVar (num, worker) = loop
                                                 where loop = do task <- takeMVar tasksMVar
                                                                 case task of
                                                                   x:xs -> do putMVar tasksMVar xs
                                                                              results <- try (invoke (num, worker) x)
                                                                              putMVar resultsMVar results
                                                                              loop
                                                                   [] -> return ()

submitWork' :: (Show a, Read b) => [String] -> [a] -> IO [b]
submitWork' workers work = do let tasks = makeTasks 10 (zip [1..] work)
                              tasksMVar <- newMVar tasks
                              resultsMVar <- newEmptyMVar
                              mapM_ (forkIO . workerLoop tasksMVar resultsMVar) (zip [1..] workers)
                              results <- replicateM (length tasks) (takeMVar resultsMVar)
                              case [ioe | Left ioe <- results] of
                                ioe:_ -> ioError ioe 
                                [] -> return
                                    $ map snd
                                    $ sortBy (comparing fst)
                                      [result | Right results' <- results, result <- results']

data (Show a, Read b) => Coordinator a b = Coordinator { submitWork :: [String] -> [a] -> IO [b],
                                                         runWorker :: IO () }

coordinator :: (Read a, Show a, Read b, Show b) => (a -> b) -> Coordinator a b
coordinator worker = Coordinator { submitWork = submitWork',
                                   runWorker = interact (show . map worker . read) }
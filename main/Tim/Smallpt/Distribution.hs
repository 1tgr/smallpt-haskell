module Tim.Smallpt.Distribution (Coordinator(..), coordinator) where

import Control.Concurrent
import Control.Exception (evaluate, try, finally)
import Control.Monad
import Data.Ord
import Data.List
import System
import System.IO
import System.Process
import Text.JSON.Generic
import Tim.Smallpt.Chart

invoke :: (Data a, Data b) => String -> [(Int, a)] -> IO [(Int, b)]
invoke worker task = do (Just inh, Just outh, Just errh, pid) <- createProcess (shell worker){ std_in  = CreatePipe,
                                                                                               std_out = CreatePipe,
                                                                                               std_err = CreatePipe }

                        outMVar <- newEmptyMVar

                        out <- hGetContents outh
                        _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

                        err <- hGetContents errh
                        _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

                        let inp = encodeJSON (map snd task)
                        putStrLn inp

                        hPutStr inh inp
                        hFlush inh
                        hClose inh

                        takeMVar outMVar
                        takeMVar outMVar
                        hClose outh

                        exitCode <- waitForProcess pid

                        hFlush stdout
                        case exitCode of
                          ExitSuccess -> do putStr err
                                            return (zip (map fst task) (decodeJSON out))
                          ExitFailure _ -> fail err

makeTasks :: Int -> [a] -> [[a]]
makeTasks n tasks = case splitAt n tasks of
                      ([], _) -> []
                      (firstN, rest) -> firstN : makeTasks n rest

process :: MVar [a] -> (a -> IO ()) -> IO ()
process queueMVar m = loop
                      where loop = do queue <- takeMVar queueMVar
                                      case queue of
                                        x:xs -> do putMVar queueMVar xs
                                                   m x
                                                   loop
                                        [] -> return ()

submitWork' :: (Data a, Data b) => [String] -> [a] -> IO [b]
submitWork' workers work = do chart <- makeChart
                              let tasks = makeTasks 10 (zip [1..] work)
                              tasksMVar <- newMVar tasks
                              resultsMVar <- newEmptyMVar
                              let invoke' n worker task = do leave <- chartEnter chart n
                                                             putMVar resultsMVar =<< try (invoke worker task) `finally` leave
                              zipWithM_ (((forkIO . process tasksMVar) .) . invoke') [1 :: Int ..] workers
                              results <- replicateM (length tasks) (takeMVar resultsMVar)
                              chartSave chart
                              case [ioe | Left ioe <- results] of
                                ioe:_ -> ioError ioe 
                                [] -> return
                                    $ map snd
                                    $ sortBy (comparing fst)
                                      [result | Right results' <- results, result <- results']

data (Data a, Data b) => Coordinator a b = Coordinator { submitWork :: [String] -> [a] -> IO [b],
                                                         runWorker :: IO () }

coordinator :: (Data a, Data b) => (a -> b) -> Coordinator a b
coordinator worker = Coordinator { submitWork = submitWork',
                                   runWorker = interact (encodeJSON . map worker . decodeJSON) }
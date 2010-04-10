{-# LANGUAGE ScopedTypeVariables #-}
module Tim.Smallpt.Chart (Chart(..), makeChart) where

import Control.Applicative
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Text.Printf
import Text.XHtml
  
data Chart a = Chart { chartEnter :: a -> IO (IO ()),
                       chartSave :: IO () }

makeChart :: forall a. (Ord a, Show a) => IO (Chart a)
makeChart = do barsMVar <- newMVar (M.empty :: M.Map a (Double, [Html]))
               startTime <- getCurrentTime
               let getCurrentSecs = fromRational . toRational . flip diffUTCTime startTime <$> getCurrentTime
                   leave n t1 = do t2 <- getCurrentSecs
                                   bars <- takeMVar barsMVar
                                   let (right, cells) = M.findWithDefault (0, []) n bars
                                       bar 0 _ = Nothing
                                       bar px cssClass = Just (thediv ! [thestyle (printf "width: %dpx" (px :: Int)), theclass cssClass] << primHtml "&nbsp;")
                                       barCssClass = "bar-" ++ show n
                                       maybeCells | t1 - right < 1 = [bar 1 "blank", bar (truncate (t2 - t1) - 1) barCssClass]
                                                  | otherwise = [bar (truncate (t1 - right)) "blank", bar (truncate (t2 - t1)) barCssClass]
                                   putMVar barsMVar (M.insert n (t2, cells ++ catMaybes maybeCells) bars)
                   enter n = do t1 <- getCurrentSecs
                                return (leave n t1)
                   save = do let makeRow n (_, cells) = tr << (th << show n +++ td << concatHtml cells)
                             trs <- concatHtml . map (uncurry makeRow) . M.assocs <$> readMVar barsMVar
                             writeFile "work.html" (renderHtml (thehtml << (body << table trs)))
               return Chart { chartEnter = enter,
                              chartSave = save }

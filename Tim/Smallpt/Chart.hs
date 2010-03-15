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
makeChart = do rowsMVar <- newMVar (M.empty :: M.Map a (Double, [Html]))
               startTime <- getCurrentTime
               let getCurrentSecs = fromRational . toRational . flip diffUTCTime startTime <$> getCurrentTime
                   leave n t1 = do t2 <- getCurrentSecs
                                   rows <- takeMVar rowsMVar
                                   let (right, cells) = M.findWithDefault (0, []) n rows
                                       cell 0 _ = Nothing
                                       cell width colour = Just ((thediv ! [thestyle (printf "float: left; width: %dpx; background-color: %s" (width :: Int) (colour :: String))]) (primHtml "&nbsp;"))
                                       (cell1, cell2) | t1 - right < 1 = (cell 1 "#fff", cell (truncate (t2 - t1) - 1) "#000")
                                                      | otherwise = (cell (truncate (t1 - right)) "#fff", cell (truncate (t2 - t1)) "#000")
                                   putMVar rowsMVar (M.insert n (t2, cells ++ catMaybes [cell1, cell2]) rows)
                   enter n = do t1 <- getCurrentSecs
                                return (leave n t1)
                   save = do let makeRow n (_, cells) = tr (th (stringToHtml (show n)) +++ td (concatHtml cells))
                             trs <- concatHtml . map (uncurry makeRow) . M.assocs <$> readMVar rowsMVar
                             writeFile "work.html" (renderHtml (thehtml (body (table trs))))
               return Chart { chartEnter = enter,
                              chartSave = save }

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Data
import Data.Default
import Hack
import Hack.Contrib.Middleware.ContentLength
import Hack.Contrib.Middleware.ContentType
import Hack.Contrib.Middleware.NotFound
import Hack.Contrib.Middleware.Static
import Hack.Contrib.Middleware.URLMap
import Hack.Contrib.Utils
import Hack.Handler.Hyena
import Hack.Middleware.Jsonp
import Network.CGI.Protocol
import Text.JSON.Generic
import qualified Tim.Smallpt.Protocol.EvalWork as EvalWork
import qualified Tim.Smallpt.Protocol.MakeWork as MakeWork
import Tim.Smallpt.Protocol.Render
import Tim.Smallpt.Render

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

respond :: JSON a => Either String a -> IO Response
respond (Left error) = return (Response { status = 500, 
                                          body = B.fromString error,
                                          headers = [] })

respond (Right response) = return (Response { status = 200,
                                              body = B.fromString (encode response), 
                                              headers = [] })

jsonFromForm :: JSON a => String -> Result a
jsonFromForm body = do pairs <- mapM jsonFromPair . formDecode $ body
                       readJSON . JSObject . toJSObject $ pairs
                    where jsonFromPair (name, value) = do jsValue <- decode value
                                                          return (name, jsValue)

worker :: forall input context a b. (Data input, Data context, Data a, Data b) => (input -> [(context, [a])]) -> (input -> context -> a -> b) -> Middleware
worker makeWork evalWork = url_map [("/work/index", respond . workIndex),
                                    ("/work/eval",  respond . workEval)]
                           where workIndex :: Env -> Either String (MakeWork.Response JSValue JSValue)
                                 workIndex env = 
                                   let rqBody = B.toString (hackInput env)
                                   in case (do request' <- jsonFromForm rqBody
                                               input' <- fromJSON (MakeWork.input request')
                                               let work' = map (\(a, b) -> MakeWork.WorkItem { MakeWork.context = toJSON a, MakeWork.task = map toJSON b}) (makeWork input')
                                               return (MakeWork.Response { MakeWork.work = work' })) of
                                      Ok response -> Right response
                                      Error s -> Left (s ++ "\n\"" ++ rqBody ++ "\"")

                                 workEval :: Env -> Either String (EvalWork.Response JSValue)
                                 workEval env = 
                                   let rqBody = B.toString (hackInput env)
                                   in case (do request' <- jsonFromForm rqBody
                                               input' <- fromJSON (EvalWork.input request')
                                               context' <- fromJSON (EvalWork.context request')
                                               task' <- mapM fromJSON (EvalWork.task request')
                                               let result' = map (toJSON . evalWork input' context') task'
                                               return (EvalWork.Response { EvalWork.result = result' })) of
                                     Ok response -> Right response
                                     Error s -> Left (s ++ "\n\"" ++ rqBody ++ "\"")

main :: IO ()
main = runWithConfig (def { port = 8080 }) $ content_length
                                           $ content_type "text/plain"
                                           $ jsonp
                                           $ worker make eval
                                           $ static (Just "/Users/tim/Git/smallpt/render") ["/"]
                                           $ not_found
                                           $ empty_app
       where make inputs = makeWork (width inputs) (height inputs) (samples inputs) (scene inputs)
             eval inputs context y = map (fmap toInt) (line inputs context y :: [Vec Double]) :: [Vec Int]
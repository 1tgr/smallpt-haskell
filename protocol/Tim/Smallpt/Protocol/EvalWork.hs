module Tim.Smallpt.Protocol.EvalWork (
  Request(..),
  Response(..),
  ) where

import Text.JSON

data Request input context a = Request { input :: input, 
                                         context :: context, 
                                         task :: [a] }

data Response b = Response { result :: [b] }

instance (JSON input, JSON context, JSON a) => JSON (Request input context a) where
  showJSON obj = makeObj [("input",   showJSON (input   obj)),
                          ("context", showJSON (context obj)),
                          ("task",    showJSON (task    obj))]

  readJSON (JSObject obj) = do input   <- valFromObj "input"   obj
                               context <- valFromObj "context" obj
                               task    <- valFromObj "task"    obj
                               return (Request { input   = input,
                                                 context = context,
                                                 task    = task })

  readJSON _ = Error "Unable to read EvalWork.Request"

instance JSON b => JSON (Response b) where
  showJSON obj = makeObj [("result", showJSON (result obj))]

  readJSON (JSObject obj) = do result <- valFromObj "result" obj
                               return (Response { result = result })

  readJSON _ = Error "Unable to read EvalWork.Response"

module Tim.Smallpt.Protocol.MakeWork (
  WorkItem(..),
  Request(..),
  Response(..),
  ) where

import Text.JSON

data WorkItem context a = WorkItem { context :: context, task :: [a] }
data Request input = Request { input :: input }
data Response context a = Response { work :: [WorkItem context a] }

instance (JSON context, JSON a) => JSON (WorkItem context a) where
  showJSON obj = makeObj [("context", showJSON (context obj)),
                          ("task",    showJSON (task    obj))]

  readJSON (JSObject obj) = do context <- valFromObj "context" obj
                               task    <- valFromObj "task"    obj
                               return (WorkItem { context = context, task = task })

  readJSON _ = Error "Unable to read MakeWork.WorkItem"

instance (JSON input) => JSON (Request input) where
  showJSON obj = makeObj [("input", showJSON (input obj))]

  readJSON (JSObject obj) = do input <- valFromObj "input" obj
                               return (Request { input = input })

  readJSON _ = Error "Unable to read MakeWork.Request"

instance (JSON context, JSON a) => JSON (Response context a) where
  showJSON obj = makeObj [("work", showJSON (work obj))]

  readJSON (JSObject obj) = do work <- valFromObj "work" obj
                               return (Response { work = work })

  readJSON _ = Error "Unable to read MakeWork.Response"

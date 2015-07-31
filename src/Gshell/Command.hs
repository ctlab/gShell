module Gshell.Command ( Command(..)
                      , Options(..)
                      ) where

data Command = Init FilePath
             | Enter FilePath
             | EnterRevision FilePath
             | Clear FilePath
             | Commit String
             | Push
             | Pull
             | Rollback
             | Log
             | Makefile (Maybe FilePath)
             | GetGraph deriving ( Show )

data Options = Options Command FilePath deriving Show

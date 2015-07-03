module Gshell.Command ( Command(..)
                      ) where

data Command = Init
             | Enter
             | EnterRevision FilePath
             | Clear
             | Commit String
             | Push
             | Pull
             | Rollback
             | Log
             | GetGraph deriving ( Show )

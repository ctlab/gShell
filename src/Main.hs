module Main where

import           Gshell.Command
import           Gshell.Run
import           Gshell.State

import           Data.String        (fromString)

import           System.Directory
import           System.Environment (getArgs)


main :: IO ()
main = do
    args' <- getArgs
    let args = map fromString args'
        [command, path'] = take 2 args
    path <- createDirectoryIfMissing True path' >> canonicalizePath path'
    res <- case command of
      "init"   -> run Init path
      "enter"  -> run Enter path
      "clear"  -> run Clear path
      "push"   -> run Push path
      "pull"   -> run Pull path
      "commit" -> run (Commit $ args !! 2) path
      "checkout" -> run (EnterRevision $ args !! 2) path
      "enterRev" -> run (EnterRevision $ args !! 2) path
      "log"    -> run Log path
      "graph"    -> run GetGraph path
      _        -> error "invalid command"
    case res of
         Right r -> case r of
                         ResultInfo info -> mapM_ putStrLn info
                         ResultPath path -> putStrLn path
                         ResultCommand command -> putStrLn $ "Success: " ++ show command
         _ -> print res

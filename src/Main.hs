{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Run

import           Data.String        (fromString)

import           System.Directory
import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    let args = map fromString args
        [command, path'] = take 2 args
    path <- createDirectoryIfMissing True path' >> canonicalizePath path'
    res <- case command of
      "init"  -> run Init path
      "enter"  -> run Enter path
      "clear"  -> run Clear path
      "commit"  -> run (Commit $ args !! 2) path
      _       -> error "invalid command"
    print res

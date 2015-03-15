{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Run

import           Data.String        (fromString)

import           System.Directory
import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    let [command, path'] = map fromString args
    path <- canonicalizePath path'
    res <- case command of
      "init"  -> run Init path
      _       -> error "invalid command"
    print res

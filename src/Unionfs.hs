module Unionfs ( unmountWorkspace
               , createWorkspace
               ) where

import           Debug
import           Folders
import           State

import           System.Exit
import           System.Process

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           System.Unix.Mount

import           System.Directory.Tree
import           System.FilePath.Posix

import           Data.List

unionfs :: FilePath
unionfs = "unionfs"

ufoptions :: [String]
ufoptions = ["-ocow", "-orelaxed_permissions"]

fuserumount :: FilePath
fuserumount = "fusermount"

fuuoptions :: [String]
fuuoptions = ["-uz"]

unmountWorkspace :: FilePath -> FilePath -> IO Result
unmountWorkspace path folder = runEitherT $ do
    mounted <- lift $ isMountPoint toUmount
    result <- if mounted
        then lift $ unmountWorkspace' $ toUmount
        else return $ Right $ toUmount ++ " is not mounted"
    case result of
        Right b -> return b
        Left b -> left b
    where
        toUmount = path </> folder

unmountWorkspace' :: FilePath -> IO Result
unmountWorkspace' workspace = do
    let options = fuuoptions ++ [workspace]
    printDebug options
    runWithExitCodeMessage fuserumount options

createWorkspace :: FilePath -> GState -> IO Result
createWorkspace workingFolder state = do
    let folders = state ^.. commitsRoot.traverse._name
        rootFolder = projectPath state
    printDebug folders
    createWorkspace' rootFolder folders workingFolder

runWithExitCodeMessage :: FilePath -> [String] -> IO Result
runWithExitCodeMessage proc options = do
    processHandle <- spawnProcess proc options
    exitCode <- waitForProcess processHandle
    case exitCode of
         ExitSuccess -> return $ Right $ show proc ++ " " ++ (last options)
         ExitFailure i -> return $ Left $ show proc ++ " exit code: " ++ show i

makeFolders :: [FilePath] -> [String]
makeFolders folders = [intercalate ":" $ head' ++ tail']
    where
        head' = [last folders ++ "=RW"]
        tail' = zipWith (\folder _ -> folder ++ "=RO") folders [1..(length folders) - 1]

createWorkspace' :: FilePath -> [FilePath] -> FilePath -> IO Result
createWorkspace' rootFolder folders workspace = do
    let folders' = makeFolders $ map (cf rootFolder </> ) folders
    let options = ufoptions ++ folders' ++ [workspace]
    runWithExitCodeMessage unionfs options
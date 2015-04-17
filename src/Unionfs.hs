module Unionfs ( unmountWorkspace
               , createWorkspace
               , unmountWorkspaces
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
import           Data.Either

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

runWithExitCodeMessage :: FilePath -> [String] -> IO Result
runWithExitCodeMessage proc options = do
    printDebug proc
    printDebug options
    processHandle <- spawnProcess proc options
    exitCode <- waitForProcess processHandle
    case exitCode of
         ExitSuccess -> return $ Right $ show proc ++ " " ++ (last options)
         ExitFailure i -> return $ Left $ show proc ++ " exit code: " ++ show i

unmountWorkspaces :: GState -> IO Result
unmountWorkspaces state = do
    let path = projectPath state
    result <- mapM (unmountWorkspace . (path </>)) $ state ^.. workFolders._name
    if (null $ result ^..below _Right)
       then return $ Left $ concat $ intersperse ", " $ lefts result
       else return $ Right $ concat $ intersperse ", " $ rights result

unmountWorkspace :: FilePath -> IO Result
unmountWorkspace toUmount = runEitherT $ do
    mounted <- lift $ isMountPoint toUmount
    result <- if mounted
        then lift $ unmountWorkspace' $ toUmount
        else return $ Right $ toUmount ++ " is not mounted"
    case result of
        Right b -> return b
        Left b -> left b

unmountWorkspace' :: FilePath -> IO Result
unmountWorkspace' workspace = do
    let options = fuuoptions ++ [workspace]
    runWithExitCodeMessage fuserumount options

createWorkspace :: FilePath -> GState -> IO Result
createWorkspace workingFolder state = do
    let folders = state ^.. commitsRoot.traverse._name
        rootFolder = projectPath state
    createWorkspace' rootFolder folders workingFolder

makeFolders :: [FilePath] -> [String]
makeFolders folders = [intercalate ":" $ head' ++ tail']
    where
        head' = [last folders ++ "=RW"]
        tail' = zipWith (\folder _ -> folder ++ "=RO") (reverse $ init folders) [1..]

createWorkspace' :: FilePath -> [FilePath] -> FilePath -> IO Result
createWorkspace' rootFolder folders workspace = do
    let folders' = makeFolders $ map (flip (</>) mountFolderName . (cf rootFolder </>)) folders
    let options = ufoptions ++ folders' ++ [workspace]
    runWithExitCodeMessage unionfs options

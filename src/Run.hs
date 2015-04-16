module Run ( Command(..)
           , initGShell
           , enterGshell
           , clearGshell
           , run
           ) where

import           Folders
import           State
import           Unionfs

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Data.List

import           Debug

data Command = Init
             | Enter
             | Clear
             | Commit String deriving ( Show )

writeStateToDisk :: StateT (GState) IO (AnchoredDirTree ())
writeStateToDisk = get >>= lift . writeDirectory

createCommitFolder :: StateT GState IO ()
createCommitFolder = do
    hash <- lift generateHash
    commitsRoot %= (++ initCommit hash)
    where initCommit hash = [
            Dir (revFolderName ++ hash) [
                Dir mountFolderName [] ] ]

initGShell :: FilePath -> StateT GState IO Result
initGShell path = do
    projectRoot .= initStructure
    createCommitFolder
    writeStateToDisk
    return $ Right path
    where initStructure = [
            Dir gshellFolderName [
                Dir commitsFolderName [] ] ]

enterGshell :: FilePath -> StateT GState IO Result
enterGshell path = do
    userId <- lift generateId
    projectRoot %= (++ initWork userId) --TODO create addState* functions?
    writeStateToDisk
    get >>= lift . createWorkspace ((wf path) ++ userId) >>= return
    where initWork userId = [
            Dir (workFolderName ++ userId) [] ]

clearGshell :: FilePath -> StateT GState IO Result
clearGshell path = do
    result <- get >>= lift . unmountWorkspaces
    lift $ removeDirectoryRecursive $ gf path
    return result

commitGshell :: String -> FilePath -> StateT GState IO Result
commitGshell message currentWork = do
    --TODO need to go up until we find .gshell :(
    modify shrinkToGshellOnly
    lift $ unmountWorkspace currentWork 
    --TODO where do we have to write message?
    createCommitFolder
    writeStateToDisk
    get >>= lift . createWorkspace (currentWork) >>= return

run :: Command -> FilePath -> IO Result
run command path' = do
    let (path, currentWork) = fixPath path'
    printDebug path
    state <- generateState path
    let existGshellRoot = not $ null $ state ^. gshellRoot
    (result, newState) <- case command of
        Init  | not existGshellRoot -> runStateT (initGShell path) state
        Init  | existGshellRoot     -> return (Left "gshell is already inited", state)
        Enter | existGshellRoot     -> runStateT (enterGshell path) state
        Enter | not existGshellRoot -> return (Left "gshell is not inited", state)
        Clear | existGshellRoot     -> runStateT (clearGshell path) state
        Clear | not existGshellRoot -> return (Left "gshell is not inited", state)
        (Commit message) | existGshellRoot -> runStateT (commitGshell message currentWork) state
        (Commit _) | not existGshellRoot -> return (Left "gshell is not inited", state)
    return result
    where fixPath path = if ((take (length workFolderName) $ takeFileName path) == workFolderName) then (takeDirectory path, path) else (path, path) --TODO maybe find .gshell here?



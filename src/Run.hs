module Run ( Command(..)
           , initGShell
           , enterGshell
           , clearGshell
           , run
           ) where

import           Names
import           State
import           Unionfs

import           Debug

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Data.List

data Command = Init
             | Enter
             | Clear
             | Commit String deriving ( Show )

writeStateToDisk :: StateT (GState) IO (AnchoredDirTree ())
writeStateToDisk = get >>= lift . writeDirectory

createCommitDir :: StateT GState IO ()
createCommitDir = do
    hash <- lift generateHash
    commitsRoot %= (++ initCommit hash)
    where initCommit hash = [
            Dir (revDirName ++ hash) [
                Dir mountDirName [] ] ]

initGShell :: FilePath -> StateT GState IO Result
initGShell path = do
    projectRoot .= initStructure
    createCommitDir
    writeStateToDisk
    return $ Right path
    where initStructure = [
            Dir gshellDirName [
                Dir commitsDirName [] ] ]

enterGshell :: FilePath -> StateT GState IO Result
enterGshell path = do
    userId <- lift generateId
    let workName = initWork userId
    projectRoot %= (++ workName) --TODO create addState* functions?
    gshellRoot %= (++ workName) --TODO create addState* functions?
    writeStateToDisk
    get >>= lift . createWorkspace ((workDir path) ++ userId) >>= return
    where initWork userId = [
            Dir (workDirName ++ userId) [] ]

clearGshell :: FilePath -> StateT GState IO Result
clearGshell path = do
    result <- get >>= lift . unmountWorkspaces
    lift $ removeDirectoryRecursive $ gshellDir path
    return result

commitGshell :: String -> FilePath -> StateT GState IO Result
commitGshell message currentWork = do
    --TODO need to go up until we find .gshell :(
    modify shrinkToGshellOnly
    lift $ unmountWorkspace currentWork
    --TODO where do we have to write message?
    createCommitDir
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
    where fixPath path = if ((take (length workDirName) $ takeFileName path) == workDirName) then (takeDirectory path, path) else (path, path) --TODO maybe find .gshell here?



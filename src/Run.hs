module Run ( Command(..)
           , initGshell
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
             | Commit String
             | Log deriving ( Show )

writeStateToDisk :: StateT (GState) IO (AnchoredDirTree ())
writeStateToDisk = get >>= lift . writeDirectory

createCommitDir :: StateT GState IO FilePath
createCommitDir = do
    hash <- lift generateHash
    let revName = revDirName ++ hash
    commitsRoot %= (++ initCommit revName)
    return revName 
    where initCommit revName = [
            Dir revName [
                Dir mountDirName [] ] ]

initGshell :: FilePath -> StateT GState IO Result
initGshell path = do
    projectRoot .= initStructure
    writeStateToDisk
    return $ Right [path]
    where initStructure = [
            Dir gshellDirName [
                Dir commitsDirName [] ] ]

enterGshell :: FilePath -> StateT GState IO Result
enterGshell path = do
    --TODO create new commit to write
    createCommitDir
    userId <- lift generateId
    folders <- gets $ flip (^..) (commitsRoot.traverse._name)
    let workState = WorkingState folders
    projectRoot %= (++ initWork userId) --TODO create addState* functions?
    gshellRoot %= (++ initWorkHelper userId workState)
    writeStateToDisk
    get >>= lift . createWorkspace ((workDir path) ++ userId) folders >>= return
    where initWork userId = [
            Dir (workDirName ++ userId) [] ]
          initWorkHelper userId workState = [
            Dir (workDirName ++ userId) [
                File workHelperFileName (show workState) ] ]

clearGshell :: FilePath -> StateT GState IO Result
clearGshell path = do
    result <- get >>= lift . unmountWorkspaces
    lift $ removeDirectoryRecursive $ gshellDir path
    return result

writeCommitMessage :: String -> FilePath -> StateT GState IO ()
writeCommitMessage message revFolder = do
    revisionRoot revFolder %= (++ [File commitFileName message])

commitGshell :: String -> FilePath -> StateT GState IO Result
commitGshell message currentWork = do
    modify shrinkToGshellOnly
    let workName = takeFileName currentWork
    lift $ unmountWorkspace currentWork
    workState <- gets $ read . view (workingState workName)
    writeCommitMessage message $ last $ workState ^. revisions
    revName <- createCommitDir
    let workState' = workState & revisions %~ (++ [revName])
    workingState workName .= show workState'
    writeStateToDisk
    get >>= lift . createWorkspace (currentWork) (workState' ^. revisions) >>= return

---FIX log is in random order
logGshell :: FilePath -> StateT GState IO Result
logGshell path = do
    history <- gets $ toListOf commitsContents
    return $ Right history

-- return project root and current work directory
findProjectRoot :: FilePath -> (FilePath, FilePath)
findProjectRoot "/" = error "No project, no work"
findProjectRoot path | isPrefixOf workDirName $ takeFileName path = (takeDirectory path, path)
findProjectRoot path | isInfixOf workDirName path = findProjectRoot $ takeDirectory path
findProjectRoot path = (path, path) --TODO bad bad bad idea

run :: Command -> FilePath -> IO Result
run command path' = do
    let (path, currentWork) = findProjectRoot path'
    printDebug path
    state <- generateState path
    let existGshellRoot = not $ null $ state ^. gshellRoot
    (result, newState) <- case command of
        Init  | not existGshellRoot -> runStateT (initGshell path) state
        Init  | existGshellRoot     -> return (Left $ gshellInited existGshellRoot, state)
        Enter | existGshellRoot     -> runStateT (enterGshell path) state
        Enter | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Clear | existGshellRoot     -> runStateT (clearGshell path) state
        Clear | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Log | existGshellRoot     -> runStateT (logGshell path) state
        Log | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        (Commit message) | existGshellRoot -> runStateT (commitGshell message currentWork) state
        (Commit _) | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
    printDebug newState
    return result

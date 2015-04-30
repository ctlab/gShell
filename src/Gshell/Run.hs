module Gshell.Run ( Command(..)
                  , initGshell
                  , enterGshell
                  , clearGshell
                  , run
                  ) where

import           Gshell.Names
import           Gshell.State
import           Gshell.Unionfs

import           Utility.Debug

import           Control.Applicative
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
             | EnterRevision FilePath
             | Clear
             | Commit String
             | Push
             | Pull
             | Log deriving ( Show )

writeStateToDisk :: StateT GState IO (AnchoredDirTree ())
writeStateToDisk = get >>= lift . writeDirectory

createCommitDir :: Parents -> StateT GState IO FilePath
createCommitDir parents = do
    hash <- lift generateHash
    let revName = revDirName ++ hash
    commitsRoot %= (++ initCommit revName)
    return revName
    where initCommit revName = [
            Dir revName [
                  Dir mountDirName []
                , File parentsFileName (show parents) ] ]

initGshell :: FilePath -> StateT GState IO Result
initGshell path = do
    projectRoot .= initStructure
    revName <- createCommitDir $ Parents []
    masterState .= revName
    writeStateToDisk
    return $ Right [path]
    where initStructure = [
            Dir gshellDirName [
                Dir commitsDirName [
                    File masterFileName [] ] ] ]

enterGshell :: FilePath -> Maybe FilePath -> StateT GState IO Result
enterGshell path revName = do
    userId <- lift generateId
    folders <- gets $ generateBranch =<< pure . maybe (view masterState) const revName
    let workState = WorkingState folders
    projectRoot %= (++ initWork userId)
    gshellRoot %= (++ initWorkHelper userId workState)
    writeStateToDisk
    get >>= lift . createWorkspace (workDir path ++ userId) folders
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
writeCommitMessage message revFolder =
    revisionRoot revFolder %= (++ [File commitFileName message])

getWorkState :: FilePath -> StateT GState IO WorkingState
getWorkState currentWork = do
    let workName = takeFileName currentWork
    gets $ read . view (workingState workName)

commitGshell :: String -> FilePath -> StateT GState IO Result
commitGshell message currentWork = do
    modify shrinkToGshellOnly
    lift $ unmountWorkspace currentWork
    workState <- getWorkState currentWork
    let parent = last $ workState ^. revisions
    writeCommitMessage message parent
    revName <- createCommitDir $ Parents [parent]
    workState' <- gets $ WorkingState . generateBranch [revName]
    workingState (takeFileName currentWork) .= show workState'
    writeStateToDisk
    get >>= lift . createWorkspace currentWork (workState' ^. revisions)

pushGshell :: FilePath -> StateT GState IO Result
pushGshell currentWork = do
    workState <- getWorkState currentWork
    let mainParent = last $ workState ^. revisions
    oldParent <- use masterState
    revName <- createCommitDir $ Parents [mainParent, oldParent]
    masterState .= revName
    writeStateToDisk
    return $ Right [revName]

pullGshell :: FilePath -> StateT GState IO Result
pullGshell currentWork = do
    modify shrinkToGshellOnly
    lift $ unmountWorkspace currentWork
    workState <- getWorkState currentWork
    let mainParent = last $ workState ^. revisions
    oldParent <- use masterState
    revName <- createCommitDir $ Parents [mainParent, oldParent]
    writeCommitMessage (mergeOf [mainParent, oldParent]) revName
    newRevName <- createCommitDir $ Parents [revName]
    workState' <- gets $ WorkingState . generateBranch [newRevName]
    workingState (takeFileName currentWork) .= show workState'
    lift $ printDebug workState'
    writeStateToDisk
    get >>= lift . createWorkspace currentWork (workState' ^. revisions)

logGshell :: FilePath -> StateT GState IO Result
logGshell currentWork = do
    revs <- view revisions <$> getWorkState currentWork
    history <- gets $ toListOf (commitsContents revs)
    return $ Right history

-- return project root and current work directory
findProjectRoot :: FilePath -> (FilePath, FilePath)
findProjectRoot "/" = error "No project, no work"
findProjectRoot path | isPrefixOf workDirName $ takeFileName path = (takeDirectory path, path)
findProjectRoot path | workDirName `isInfixOf` path = findProjectRoot $ takeDirectory path
findProjectRoot path = (path, path) --TODO bad bad bad idea

run :: Command -> FilePath -> IO Result
run command path' = do
    let (path, currentWork) = findProjectRoot path'
    state <- generateState path
    printDebug state
    let existGshellRoot = not $ null $ state ^. gshellRoot
    (result, newState) <- case command of
        Init  | not existGshellRoot -> runStateT (initGshell path) state
        Init  | existGshellRoot     -> return (Left $ gshellInited existGshellRoot, state)
        Enter | existGshellRoot     -> runStateT (enterGshell path Nothing) state
        Enter | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        (EnterRevision revName) | existGshellRoot -> runStateT (enterGshell path $ Just revName) state
        (EnterRevision _) | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Clear | existGshellRoot     -> runStateT (clearGshell path) state
        Clear | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Log | existGshellRoot     -> runStateT (logGshell currentWork) state
        Log | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Push | existGshellRoot     -> runStateT (pushGshell currentWork) state
        Push | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        Pull | existGshellRoot     -> runStateT (pullGshell currentWork) state
        Pull | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
        (Commit message) | existGshellRoot -> runStateT (commitGshell message currentWork) state
        (Commit _) | not existGshellRoot -> return (Left $ gshellInited existGshellRoot, state)
    printDebug newState
    return result

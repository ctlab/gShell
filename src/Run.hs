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
import           Data.Either

import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Data.List

import           Debug

data Command = Init
             | Enter
             | Clear
             | Commit String
             deriving ( Show )

writeStateToDisk :: StateT (GState) IO (AnchoredDirTree ())
writeStateToDisk = get >>= lift . writeDirectory

initGShell :: FilePath -> StateT GState IO Result
initGShell path = do
    hash <- lift generateHash
    projectRoot .= initStructure hash
    writeStateToDisk
    return $ Right path
    where initStructure hash = [
            Dir gshellFolderName [
                Dir commitsFolderName [
                    Dir (revFolderName ++ hash) [] ] ] ]

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
    result <- gets  (^.. workFolders._name) >>= lift . mapM (unmountWorkspace path)
    lift $ removeDirectoryRecursive $ gf path
    if (null $ result ^..below _Right)
       then return $ Left $ concat $ intersperse ", " $ lefts result
       else return $ Right $ concat $ intersperse ", " $ rights result


commitGshell :: String -> FilePath -> StateT GState IO Result
commitGshell = commitGshell

run :: Command -> FilePath -> IO Result
run command path = do
    state <- generateState path
    let existGshellRoot = not $ null $ state ^. gshellRoot
    (result, newState) <- case command of
        Init  | not existGshellRoot -> runStateT (initGShell path) state
        Init  | existGshellRoot     -> return (Left "gshell is already inited", state)
        Enter | existGshellRoot     -> runStateT (enterGshell path) state
        Enter | not existGshellRoot -> return (Left "gshell is not inited", state)
        Clear | existGshellRoot     -> runStateT (clearGshell path) state
        Clear | not existGshellRoot -> return (Left "gshell is not inited", state)
        (Commit message) | existGshellRoot -> runStateT (commitGshell message path) state
        (Commit _) | not existGshellRoot -> return (Left "gshell is not inited", state)
    {-printDebug newState-}
    return result


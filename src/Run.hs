module Run ( Command(..)
           , Result(..)
           , initGShell
           , enterGshell
           , clearGshell
           , run
           ) where

import           Folders
import           State

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Debug

data Command = Init
             | Enter
             | Clear
             deriving ( Show )

data Result = Fail String
            | Success String
            deriving ( Show )

initGShell :: FilePath -> StateT GState IO Result
initGShell path = do
    hash <- lift generateHash
    projectRoot .= initStructure hash
    return $ Success path
    where initStructure hash = [
            Dir gshellFolderName [
                Dir commitsFolderName [
                    Dir (revFolderName ++ hash) [] ] ] ]

enterGshell :: FilePath -> StateT GState IO Result
enterGshell path = do
    userId <- lift generateId
    projectRoot %= (++ initWork userId) --TODO create addState* functions?
    return $ Success $ (wf path) ++ userId
    where initWork userId = [
            Dir (workFolderName ++ userId) [] ]

clearGshell :: FilePath -> StateT GState IO Result
clearGshell path = do
    lift $ removeDirectoryRecursive $ gf path
    return $ Success "Clear"

run :: Command -> FilePath -> IO Result
run command path = do
    state <- generateState path
    let existGshellRoot = not $ null $ state ^. gshellRoot
    (result, newState) <- case command of
        Init  | not existGshellRoot -> runStateT (initGShell path) state
        Init  | existGshellRoot     -> return (Fail "gshell is already inited", state)
        Enter | existGshellRoot     -> runStateT (enterGshell path) state
        Enter | not existGshellRoot -> return (Fail "gshell is not inited", state)
        Clear | existGshellRoot -> runStateT (clearGshell path) state
        Clear | not existGshellRoot -> return (Fail "gshell is not inited", state)
    printDebug newState
    writeDirectory newState
    return result


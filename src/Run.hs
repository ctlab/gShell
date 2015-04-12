module Run ( Command(..)
           , Result(..)
           , initGShell
           , enterGshell
           , run
           ) where
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Text                 (Text)
import qualified Data.Text.IO              as T
import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Control.Arrow
import           Data.Default
import           Folders
import           State

import           Debug

data Command = Init
             | Enter
             deriving ( Show )

data Result = Fail String
            | Success String
            deriving ( Show )

initGShell :: FilePath -> StateT GState IO Result
initGShell path = do
    currentgshellRoot <- use gshellRoot
    if (not $ null $ currentgshellRoot)
    then return $ Fail "gshell root alredy exists"
        -- printDebug "Removing current gshell root" >> removeDirectoryRecursive (gf path)
    else do
        hash <- lift generateHash
        projectRoot .= initStructure hash
        return $ Success path
        where initStructure hash = [
                Dir gshellFolderName [
                    Dir commitsFolderName [
                        Dir (revFolderName ++ hash) [] ] ] ]

enterGshell :: FilePath -> StateT GState IO Result
enterGshell path = return $ Success "Enter"
            --- ,   Dir (workFolderName ++ hash) [] we need this in enter function


run :: Command -> FilePath -> IO Result
run command path = do
    state <- generateState path
    (result, newState) <- case command of
        Init -> runStateT (initGShell path) state
        Enter -> runStateT (enterGshell path) state -- check if path and .gshell are presented
    {-print newState-}
    writeDirectoryWith T.writeFile newState
    return result


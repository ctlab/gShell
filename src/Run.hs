module Run where
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import Control.Applicative

import           System.Directory
import           System.Directory.Tree
import           System.FilePath

import           Data.Default
import           Folders
import           State
import Control.Arrow

data Command = Init
             | Enter
             deriving ( Show )

data Result = Failed String
            | Succes String
            deriving ( Show )

initGShell :: FilePath -> StateT GlobalState IO Result
initGShell path =
    lift $ do
        hash <- generateHash
        writeDirectory $ (path ++ "/..") :/ 
            Dir path [
                Dir gshellFolderName [
                    Dir commitsFolderName [
                        Dir (revFolderName ++ hash) []
                    ]
                ]
            ]
        return $ Succes $ wf path ++ hash

enterGshell :: FilePath -> StateT GlobalState IO Result
enterGshell path = return $ Succes "Enter"
            --- ,   Dir (workFolderName ++ hash) [] we need this in enter function
 

run :: Command -> FilePath -> IO Result
run command path = do
    state <- generateState path
    (result, newState) <- case command of
        Init -> runStateT (initGShell path) def
        Enter -> runStateT (enterGshell path) state -- check if path and .gshell are presented
    {-print newState-}
    return result


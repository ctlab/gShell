module Run where
import           System.FilePath
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Lens

import State
import           Folders
import Data.Default

data Command = Init
             deriving ( Show )

data Result = Failed String
            | Succes String
            deriving ( Show )

initGShell :: FilePath -> StateT GlobalState IO Result
initGShell path = return $ Succes "Hello"

run :: Command -> FilePath -> IO Result
run command path = do
    state <- generateState path
    (result, newState) <- case command of
        Init -> runStateT (initGShell path) def
    {-print newState-}
    return result


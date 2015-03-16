{-# LANGUAGE TemplateHaskell #-}

module State where

import           Folders

import           Control.Lens
import           Data.Default
import           System.Directory
import           System.Directory.Tree as DT
import           System.IO.Error

data Commit = Commit { _number       :: Int
                     , _work         :: DirTree FilePath
                     , _changedFiles :: FilePath
                     , _message      :: String
                     } deriving (Show)
makeLenses ''Commit

instance Default Commit where
    def = Commit { _number = undefined
                 , _work = undefined
                 , _changedFiles = []
                 , _message = undefined
                 }

data GshellState = GshellState { _isOn    :: Bool
                               , _commits :: [Commit]
                               } deriving (Show)
makeLenses ''GshellState

instance Default GshellState where
    def = GshellState { _isOn = False
                      , _commits = def
                      }

data GlobalState = GlobalState { _gShellState    :: GshellState
                               , _workingFolders :: [DirTree FilePath]
                               , _rootFolder     :: FilePath
                               } deriving (Show)
makeLenses ''GlobalState

instance Default GlobalState where
    def = GlobalState { _gShellState = def
                      , _workingFolders = []
                      , _rootFolder = undefined
                      }

generateState :: FilePath -> IO GlobalState
generateState path = do
    createDirectoryIfMissing True $ gf path
    myTree <- build $ gf path
    print (dirTree myTree)
    let commits = maybe (gf path :/ DT.Failed commitsFolderName (userError $ "No " ++ commitsFolderName ++ " folder")) (id) (dropTo commitsFolderName myTree)
        state = gShellState.isOn .~ True
                $ rootFolder .~ path
                $ def
    print commits
    return $ state

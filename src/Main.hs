{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import Data.List
import GHC.Generics
import System.Environment ( getArgs )
import System.Daemon
import Control.Lens
import System.Process
import System.Directory
import System.Exit
import System.FilePath
import Control.Monad

data Command = Init FilePath
               deriving ( Generic, Show )

instance Serialize Command

data Response = Failed String
              | Succes
                deriving ( Generic, Show )

instance Serialize Response

data State = State { _isOn :: Bool
                   , _revision :: Int
                   , _workingFolder :: FilePath
                   } deriving ( Generic, Show)

instance Serialize State

makeLenses ''State

gShellFolderName :: FilePath
gShellFolderName = ".gShell"

gShellRevisionFolderInfix :: FilePath
gShellRevisionFolderInfix = "rev"

gShellWorkingFolderName :: FilePath
gShellWorkingFolderName = "work"

gShellWorkingFolder :: FilePath -> FilePath
gShellWorkingFolder path = path </> gShellFolderName

makeFoldersRO :: State -> [String]
makeFoldersRO = makeFolders "RO"

makeFoldersRW :: State -> [String]
makeFoldersRW = makeFolders "RW"

makeFolders :: String -> State -> [String]
makeFolders mode state = return $ intercalate ":" 
    $ zipWith (++) (repeat $ flip (++) "/" $ state ^. workingFolder) (
        zipWith (\a b -> a ++ show b ++ "=" ++ mode) (repeat "rev") [state ^. revision, (-) 1 $ state ^. revision .. 0]
    ) 
initGShell :: State -> FilePath -> IO Response
initGShell state path = do
    let wp = gShellWorkingFolder path
    createDirectoryIfMissing True wp
    createDirectoryIfMissing True $ wp </> gShellWorkingFolderName
    createDirectoryIfMissing True $ wp </> gShellRevisionFolderInfix ++ (show $ state ^. revision)
    let folders = makeFoldersRW $ state & workingFolder .~ wp
    let workingFolder = [wp </> gShellWorkingFolderName]
    let options = ["-ocow", "-orelaxed_permissions"] ++ folders ++ workingFolder
    processHandle <- spawnProcess "unionfs" options
    exitCode <- waitForProcess processHandle
    case exitCode of 
         ExitSuccess -> return Succes
         ExitFailure i -> return $ Failed $ "Exit with " ++ (concat $ options)

handleCommand :: MVar State -> Command -> IO Response
handleCommand stateVar comm = join $ modifyMVar stateVar $ \state -> return $
    case comm of
      Init path -> (state & isOn .~ True & workingFolder .~ (gShellWorkingFolder path)
                 , initGShell state path)

port :: Int
port = 9668

emptyState :: State
emptyState = State { _isOn = False, _revision = 0, _workingFolder = [] }

runClientOnLocalhost :: (Serialize a, Serialize b) => a -> IO (Maybe b)
runClientOnLocalhost comm = runClient "localhost" port comm

main :: IO ()
main = do
    stateVar <- newMVar emptyState
    let options = def { daemonPort = port }
    ensureDaemonRunning "gShell" options (handleCommand stateVar)
    args <- getArgs
    let args' = map fromString args
    res <- case args' of
      ["init", path]      -> runClientOnLocalhost $ Init path
      _                   -> error "invalid command"
    print (res :: Maybe Response)

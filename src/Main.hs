{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import           Data.Default          (def)
import           Data.Default
import           Data.List
import           Data.String           (fromString)
import           Data.Time.Clock.POSIX
import           System.Directory
import           System.Environment    (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO.Unsafe
import           System.Process
import           System.Random


data Command = Init
             | Off
             | Clean
             | Commit
               deriving ( Show )


data Result = Failed String
            | Succes String
            | Folder FilePath
            deriving ( Show )

data State = State { _isOn            :: Bool
                   , _isOff           :: Bool
                   , _revision        :: Int
                   , _gShellDirectory :: FilePath
                   , _workingFolder   :: FilePath
                   } deriving ( Show, Read )

instance Default State where
    def = State { _isOn = False
                , _isOff = True
                , _revision = 0
                , _gShellDirectory = []
                , _workingFolder = []
                }

makeLenses ''State

directoryName :: FilePath
directoryName = ".gShell"

revisionInfix :: FilePath
revisionInfix = "rev"

generateWorkDirName :: IO FilePath
generateWorkDirName = do
    g <- getStdGen
    time <- show <$> getPOSIXTime
    let a = take 5 $ (randomRs ('a', 'z') g)
    let b = take 5 $ (randomRs ('0', '9') g)
    let hash = concat $ zipWith (\a b -> a:[b]) a b
    return $ time ++ "-" ++ hash ++ "-" ++ "work"

rootDirectory :: FilePath -> FilePath
rootDirectory path = path </> directoryName

readGShellState :: FilePath -> IO State
readGShellState path = do
    state <- readFile $ rootDirectory path </> ".state"
    return $ read state

writeState :: State -> IO ()
writeState state = writeFile (state ^. gShellDirectory </> ".state") $ show state

makeFoldersRO :: State -> [String]
makeFoldersRO = makeFolders "RO"

makeFoldersRW :: State -> [String]
makeFoldersRW = makeFolders "RW"

makeFolders :: String -> State -> [String]
makeFolders mode state = return $ intercalate ":"
    $ zipWith (++) (repeat $ flip (++) "/" $ state ^. gShellDirectory) (
        zipWith (\a b -> a ++ show b ++ "=" ++ mode) (repeat "rev") toZip
    )
    where
        toZip = case mode of
                    "RW" -> [1 + state ^. revision]
                    "RO" -> [state ^. revision, (-) 1 $ state ^. revision .. 0]

getLastCommand :: IO String
{-getLastCommand = readProcess "fc" ["-n", "-l", "-1"] []-}
getLastCommand = return "Commit MSG"

findGshellRoot :: FilePath -> IO (Maybe FilePath)
findGshellRoot "/" = return Nothing
findGshellRoot path = do
    doesExist <- doesDirectoryExist $ rootDirectory path
    if doesExist then return $ Just (unsafePerformIO $ canonicalizePath $ path) else findGshellRoot $ path </> ".."

initGShell :: State -> FilePath -> IO Result
initGShell state wp = do
    let folders = makeFoldersRW $ state & gShellDirectory .~ wp & revision -~ 1
    let workFolder =  [state ^. workingFolder]
    let options = ["-ocow", "-orelaxed_permissions"] ++ folders ++ workFolder
    processHandle <- spawnProcess "unionfs" options
    exitCode <- waitForProcess processHandle
    case exitCode of
         ExitSuccess -> return $ Succes "GShell is inited"
         ExitFailure i -> return $ Failed $ "Exit with " ++ show i

unmountWorkingFolder :: State -> IO ProcessHandle
unmountWorkingFolder state = do
    let workFolder = state ^. workingFolder
    print $ "working unmount"
    print workFolder
    let options = ["-uz", workFolder]
    spawnProcess "fusermount" options

offGShell :: State -> IO Result
offGShell state = do
    processHandle <- unmountWorkingFolder state
    exitCode <- waitForProcess  processHandle
    case exitCode of
         ExitSuccess -> return $ Succes "GShell is off"
         ExitFailure i -> return $ Failed $ "Exit with " ++ show i

commit :: State -> FilePath -> IO Result
commit state newWorkFolder = do
    unmountWorkingFolder state
    let wp = state ^. gShellDirectory
    createDirectoryIfMissing True wp
    createDirectoryIfMissing True $ wp </> newWorkFolder
    createDirectoryIfMissing True $ wp </> revisionInfix ++ (show $ 1 + state ^. revision)
    lastCommand <- getLastCommand
    writeFile (wp </> ("rev" ++ show (state ^. revision)) </> "commit_msg") lastCommand
    let foldersRO = makeFoldersRO $ state & gShellDirectory .~ wp
    let foldersRW = makeFoldersRW $ state & gShellDirectory .~ wp
    let folders = [concat $ concat $ intersperse [":"] [foldersRW, foldersRO]]
    let options = ["-ocow", "-orelaxed_permissions"] ++ folders ++ [newWorkFolder]
    processHandle <- spawnProcess "unionfs" options
    exitCode <- waitForProcess processHandle
    case exitCode of
         ExitSuccess -> return $ Succes "GSHell commited"
         ExitFailure i -> return $ Failed $ "Exit with " ++ show i


run :: Command -> FilePath -> IO Result
run comm path = do
    existGShellFolder <- doesDirectoryExist $ rootDirectory path
    existGShellState <- doesFileExist $ rootDirectory path </> ".state"
    case comm of
        Init -> do -- GUARDS
                if existGShellState
                then return $ Failed "Gshell is already inited"
                else do
                    workFolder <- ((</>) (rootDirectory path)) <$> generateWorkDirName
                    let state = def { _isOn = True, _gShellDirectory = rootDirectory path, _isOff = False, _workingFolder = workFolder }
                    let wp = state ^. gShellDirectory
                    createDirectoryIfMissing True wp
                    createDirectoryIfMissing True $ wp </> workFolder
                    createDirectoryIfMissing True $ wp </> revisionInfix ++ (show $ state ^. revision)
                    result <- initGShell state wp
                    writeState state
                    {-print $ result --TODO What to do with it-}
                    return $ Folder workFolder
        Off -> do
               if existGShellState
               then do
                   state <- readGShellState path
                   if state ^. isOff
                   then
                       return $ Failed "GShell is already off"
                   else do
                       result <- offGShell state
                       writeState $ state & isOn .~ False & isOff .~ True
                       return result
               else return $ Failed "GShell is not inited"
        Clean -> do
            if existGShellFolder
            then do
                removeDirectoryRecursive $ rootDirectory path
                return $ Succes "GSHell directory is clean"
            else return $ Failed "Gshell is not inited"
        Commit -> do -- Check if on
            (Just path') <- findGshellRoot path
            state <- readGShellState path'
            if state ^. isOn
            then do
                workFolder <- ((</>) (rootDirectory path')) <$> generateWorkDirName
                result <- commit state workFolder
                {-print $ result --TODO What to do with it-}
                writeState $ state & revision +~ 1 & workingFolder .~ workFolder
                return $ Folder workFolder
            else return $ Failed "GShell is off"


main :: IO ()
main = do
    args <- getArgs
    path <- getCurrentDirectory
    let args' = map fromString args
    res <- case args' of
      ["init"]  -> run Init path
      ["off"]   -> run Off path
      ["clean"] -> run Clean path
      ["commit"] -> run Commit path
      _         -> error "invalid command"
    case res of
         Folder name -> print name
         a@(Succes _) -> print a
         b@(Failed _) -> print b

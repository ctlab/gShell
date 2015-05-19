module Gshell.Unionfs ( unmountWorkspace
                      , createWorkspace
                      , unmountWorkspaces
                      , generateBranch
                      ) where

import           Utility.Debug
import           Gshell.Names
import           Gshell.State

import           System.Exit
import           System.Process

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Either
import           System.Unix.Mount

import           System.Directory.Tree
import           System.FilePath.Posix
import           System.FilePath

import           Data.List

unionfs :: FilePath
unionfs = "unionfs"

ufoptions :: FilePath -> FilePath -> [String]
ufoptions path fullWorkDirName = ["-ocow", "-orelaxed_permissions", "-odebug_file=" ++ unionfsLogFile path fullWorkDirName]

fusermount :: FilePath
fusermount = "fusermount"

fuuoptions :: [String]
fuuoptions = ["-uz"]

runWithExitCodeMessage :: FilePath -> [String] -> IO Result
runWithExitCodeMessage proc options = do
    (exitCode, stdo, stde) <- readProcessWithExitCode proc options ""
    case exitCode of
         ExitSuccess -> return $ Right $
            ResultInfo $ [show proc ++ " " ++ (last options)]
         ExitFailure i -> return $ Left $
            show proc ++ " exit code: " ++ show i
                ++ "\nstdout: " ++ stdo
                ++ "\nstderr: " ++ stde

unmountWorkspaces :: GState -> IO Result
unmountWorkspaces state = do
    let path = projectPath state
    result <- mapM (unmountWorkspace . (path </>)) $ state ^.. workDirs._name
    if (null $ result ^..below _Right)
       then return $ Left $ concat $ intersperse ", " $ lefts result
       else return $ Right $ ResultInfo $ concatMap (\(ResultInfo a) -> a) $ rights result

unmountWorkspace :: FilePath -> IO Result
unmountWorkspace toUmount = runEitherT $ do
    mounted <- lift $ isMountPoint toUmount
    result <- if mounted
        then lift $ unmountWorkspace' $ toUmount
        else return $ Right $ ResultInfo $ [toUmount ++ " is not mounted"]
    case result of
        Right b -> return b
        Left b -> left b

unmountWorkspace' :: FilePath -> IO Result
unmountWorkspace' workspace = do
    let options = fuuoptions ++ [workspace]
    runWithExitCodeMessage fusermount options

createWorkspace :: FilePath -> [FilePath] -> GState -> IO Result
createWorkspace workingDir folders state = do
    let rootDir = projectPath state
    createWorkspace' rootDir folders workingDir

generateBranch :: [String] -> GState -> [FilePath]
generateBranch revs state = result
    where
        result = sortBy cmp $ nub $ concatMap (generateBranch' state) revs
        cmp rev1 rev2 = (state ^. timeStamp rev1) `compare` (state ^. timeStamp rev2)

generateBranch' :: GState -> String -> [FilePath]
generateBranch' state rev = rev : otherParents
    where
        allParents = (read $ state ^. parents rev) ^. parentsRevs
        otherParents = if null allParents then [] else concatMap (generateBranch' state) allParents

makeDirs :: [FilePath] -> [String]
makeDirs folders = [intercalate ":" $ head' ++ tail']
    where
        head' = [last folders ++ "=RW"]
        tail' = map (++ "=RO") (reverse $ init folders)

createWorkspace' :: FilePath -> [FilePath] -> FilePath -> IO Result
createWorkspace' rootDir folders workspace = do
    let folders' = makeDirs $ map (flip (</>) mountDirName . (commitsDir rootDir </>)) $ folders
    let options = ufoptions rootDir (takeFileName workspace) ++ folders' ++ [workspace]
    runWithExitCodeMessage unionfs options

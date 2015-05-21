{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gshell.State ( GState (..)
                    , GDir (..)
                    , Results (..)
                    , Result (..)
                    , WorkingState (..)
                    , Parents (..)
                    , OpenedFiles
                    , parentsRevs
                    , parents
                    , revisions
                    , projectPath
                    , projectRoot
                    , gshellRoot
                    , commitsRoot
                    , revisionRoot
                    , workDirs
                    , workingState
                    , unionfsLog
                    , masterState
                    , timeStamp
                    , readLog
                    , getReadLog
                    , revCommit
                    , generateState
                    , cmpRevs
                    ) where

import           Gshell.Command
import           Gshell.Names

import           Control.Applicative
import           Control.Lens
import           System.Directory
import           System.Directory.Tree
import           System.FilePath.Posix

import           Data.List
import qualified Data.Map              as M

type GState = AnchoredDirTree String
type GDir = DirTree String

data Results = ResultPath FilePath
             | ResultCommand Command
             | ResultInfo [String]
             deriving Show

type Result = Either String Results

data WorkingState = WorkingState { _revisions :: [FilePath]
                                 } deriving (Show, Read)

makeLenses ''WorkingState

data Parents = Parents { _parentsRevs :: [FilePath]
                       } deriving (Show, Read)

makeLenses ''Parents

type OpenedFiles = M.Map FilePath [FilePath]

projectPath :: GState -> FilePath
projectPath state = state ^. _anchor </> state ^. _dirTree._name

filteredByName
  :: (Choice p, Applicative f) =>
     FileName -> Optic' p f (GDir) (GDir)
filteredByName = filtered . (. view _name) . isPrefixOf

filteredByNames
  :: (Applicative f, Choice p) =>
     [FileName] -> Optic' p f (DirTree a) (DirTree a)
filteredByNames = filtered . flip (any . isPrefixOf . view _name)

projectRoot :: Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
projectRoot = _dirTree._contents

gshellRoot :: Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
gshellRoot = projectRoot.traverse.filteredByName gshellDirName._contents

commitsRoot :: Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
commitsRoot = gshellRoot.traverse.filteredByName commitsDirName._contents

revisionRoot :: Applicative f =>
     FileName
     -> ([DirTree String] -> f [DirTree String])
     -> GState -> f GState
revisionRoot name = commitsRoot.traverse.filteredByName name._contents

workDirs :: Applicative f =>
         (GDir -> f (GDir))
         -> GState -> f GState
workDirs = projectRoot.traverse.filteredByName workDirName

workingState :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
workingState name = gshellRoot.traverse.filteredByName name._contents.traverse.filteredByName workHelperFileName._file

unionfsLog :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
unionfsLog name = gshellRoot.traverse.filteredByName name._contents.traverse.filteredByName unionfsLogFileName._file

masterState :: Applicative f =>
        (String -> f String)
        -> GState -> f GState
masterState = gshellRoot.traverse.filteredByName masterFileName._file

parents :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
parents name = revisionRoot name.traverse.filteredByName parentsFileName._file

timeStamp :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
timeStamp name = revisionRoot name.traverse.filteredByName timeStampFileName._file

readLog  :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
readLog name = revisionRoot name.traverse.filteredByName logFileName._file

getReadLog :: FilePath -> GState -> OpenedFiles
getReadLog revName state = do
    read $ view (readLog revName) state

revCommit :: Applicative f =>
    FileName
    -> (String -> f String)
    -> GState -> f GState
revCommit revision = commitsRoot.traverse.filteredByName revision._contents.traverse.filteredByName commitFileName._file

generateState :: FilePath -> IO GState
generateState path = do
    state <- readDirectoryWithL (\file ->
        if any (flip isPrefixOf $ takeFileName file)
            [ commitFileName
            , workHelperFileName
            , masterFileName
            , parentsFileName
            , timeStampFileName
            , unionfsLogFileName
            , logFileName
            ]
        then readFile file
        else return "") path
    return $ state & _dirTree %~ myTransformDir removeUnessesary []
    where
        removeUnessesary parent (Dir name contents)
            | workDirName `isPrefixOf` name && parent /= gshellDirName
                || name `isPrefixOf` mountDirName = Dir name []
        removeUnessesary parent c = c

myTransformDir :: (FilePath -> DirTree a -> DirTree a) -> FilePath -> DirTree a -> DirTree a
myTransformDir f p t = case f p t of
                     (Dir n cs) -> Dir n $ map (myTransformDir f n) cs
                     t'         -> t'

cmpRevs :: GState -> String -> String -> Ordering
cmpRevs state rev1 rev2 = (state ^. timeStamp rev1) `compare` (state ^. timeStamp rev2)

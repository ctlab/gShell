{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gshell.State ( GState (..)
                    , GDir (..)
                    , Result (..)
                    , WorkingState (..)
                    , Parents (..)
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
                    , masterState
                    , commitsContents
                    , generateState
                    , shrinkToGshellOnly
                    ) where

import           Gshell.Names

import           Control.Applicative
import           Control.Lens
import           System.Directory
import           System.Directory.Tree
import           System.FilePath.Posix

import           Data.List

type GState = AnchoredDirTree String
type GDir = DirTree String

type Result = Either String [String]

data WorkingState = WorkingState { _revisions :: [FilePath]
                                 } deriving (Show, Read)

makeLenses ''WorkingState

data Parents = Parents { _parentsRevs :: [FilePath]
                       } deriving (Show, Read)

makeLenses ''Parents

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

masterState :: Applicative f =>
        (String -> f String)
        -> GState -> f GState
masterState = commitsRoot.traverse.filteredByName masterFileName._file

parents :: Applicative f =>
           FileName
           -> (String -> f String)
           -> GState -> f GState
parents name = revisionRoot name.traverse.filteredByName parentsFileName._file

commitsContents :: Applicative f =>
    [FileName]
    -> (String -> f String)
    -> GState -> f GState
commitsContents revisions = commitsRoot.traverse.filteredByNames revisions._contents.traverse.filteredByName commitFileName._file

generateState :: FilePath -> IO GState
generateState path = do
    state <- readDirectoryWith (\file -> if any (flip isPrefixOf $ takeFileName file) [commitFileName, workHelperFileName, masterFileName, parentsFileName]
                                            then readFile file
                                            else return "") path
    return $ state & _dirTree %~ sortDirShape

shrinkToGshellOnly :: GState -> GState
shrinkToGshellOnly state = state & projectRoot .~ (state ^. projectRoot) \\ (state ^.. workDirs)

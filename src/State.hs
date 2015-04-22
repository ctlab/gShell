{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module State ( GState (..)
             , GDir (..)
             , Result (..)
             , WorkingState (..)
             , revisions
             , projectPath
             , projectRoot
             , gshellRoot
             , commitsRoot
             , revisionRoot
             , workDirs
             , workingState
             , commitsContents
             , generateState
             , shrinkToGshellOnly
             ) where

import           Names

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

projectPath :: GState -> FilePath
projectPath state = state ^. _anchor </> state ^. _dirTree._name

filteredByName
  :: (Choice p, Applicative f) =>
     FileName -> Optic' p f (GDir) (GDir)
filteredByName name = filtered ((== name) . (^. _name))

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
workDirs = projectRoot.traverse.filtered ((== workDirName) . (Prelude.take (Prelude.length workDirName)) . (^. _name))

workingState :: Applicative f =>
           FileName -> (String -> f String)
           -> GState -> f GState
workingState name = gshellRoot.traverse.filteredByName name._contents.traverse._file 

commitsContents :: Applicative f =>
    (String -> f String)
    -> GState -> f GState
commitsContents = commitsRoot.traverse._contents.traverse.filteredByName commitFileName._file

generateState :: FilePath -> IO GState
generateState = readDirectory

shrinkToGshellOnly :: GState -> GState
shrinkToGshellOnly state = state & projectRoot .~ (state ^. projectRoot) \\ (state ^.. workDirs)

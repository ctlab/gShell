{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module State ( GState (..)
             , GDir (..)
             , Result (..)
             , projectPath
             , projectRoot
             , gshellRoot
             , commitsRoot
             , workDirs
             , viewProjectRoot
             , viewGshellRoot
             , viewCommitsRoot
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

type Result = Either String String

projectPath :: GState -> FilePath
projectPath state = state ^. _anchor </> state ^. _dirTree._name

filteredByName
  :: (Choice p, Applicative f) =>
     FileName -> Optic' p f (GDir) (GDir)
filteredByName name = filtered ((== name) . (^. _name))

projectRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
projectRoot = _dirTree._contents

viewProjectRoot :: GState -> [GDir]
viewProjectRoot = view projectRoot

gshellRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
gshellRoot = projectRoot.traverse.filteredByName gshellDirName._contents

viewGshellRoot :: GState -> [GDir]
viewGshellRoot = view gshellRoot

commitsRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
commitsRoot = gshellRoot.traverse.filteredByName commitsDirName._contents

viewCommitsRoot :: GState -> [GDir]
viewCommitsRoot = view commitsRoot

workDirs :: Applicative f =>
         (GDir -> f (GDir))
         -> GState -> f GState
workDirs = projectRoot.traverse.filtered ((== workDirName) . (Prelude.take (Prelude.length workDirName)) . (^. _name))

generateState :: FilePath -> IO GState
generateState = readDirectory

shrinkToGshellOnly :: GState -> GState
shrinkToGshellOnly state = state & projectRoot .~ (state ^. projectRoot) \\ (state ^.. workDirs)

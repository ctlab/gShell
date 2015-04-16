{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module State ( GState (..)
             , GDir (..)
             , Result (..)
             , projectPath
             , projectRoot
             , gshellRoot
             , commitsRoot
             , workFolders
             , viewProjectRoot
             , viewGshellRoot
             , viewCommitsRoot
             , generateState
             , shrinkToGshellOnly
             ) where

import           Folders

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
gshellRoot = projectRoot.traverse.filteredByName gshellFolderName._contents

viewGshellRoot :: GState -> [GDir]
viewGshellRoot = view gshellRoot

commitsRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
commitsRoot = gshellRoot.traverse.filteredByName commitsFolderName._contents

viewCommitsRoot :: GState -> [GDir]
viewCommitsRoot = view commitsRoot

workFolders :: Applicative f =>
         (GDir -> f (GDir))
         -> GState -> f GState
workFolders = projectRoot.traverse.filtered ((== workFolderName) . (Prelude.take (Prelude.length workFolderName)) . (^. _name))

generateState :: FilePath -> IO GState
generateState = readDirectory

shrinkToGshellOnly :: GState -> GState
shrinkToGshellOnly state = state & projectRoot .~ (state ^. projectRoot) \\ (state ^.. workFolders)

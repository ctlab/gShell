{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module State ( GState (..)
             , GDir (..)
             , projectRoot
             , gshellRoot
             , commitsRoot
             , workFolders
             , viewProjectRoot
             , viewGshellRoot
             , viewCommitsRoot
             , generateState
             ) where

import           Folders

import           Control.Applicative
import           Control.Lens
import           System.Directory
import           System.Directory.Tree

type GState = AnchoredDirTree FilePath
type GDir = DirTree FilePath

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
         (GDir -> f (GDir)) -> GState -> f GState
workFolders = projectRoot.traverse.filtered ((== workFolderName) . (Prelude.take (Prelude.length workFolderName)) . (^. _name))

generateState :: FilePath -> IO GState
generateState = build

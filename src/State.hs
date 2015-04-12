{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module State ( GState (..)
             , GDir (..)
             , projectRoot
             , gshellRoot
             , commitsRoot
             , addToProject
             , addToGshell
             , addToCommits
             , viewProjectRoot
             , viewGshellRoot
             , viewCommitsRoot
             , generateState
             ) where

import           Folders

import           Control.Applicative
import           Control.Lens
import           Data.Default
import           Data.Text
import qualified Data.Text.IO          as T
import           System.Directory
import           System.Directory.Tree
import           System.IO.Error

type GState = AnchoredDirTree Text
type GDir = DirTree Text

addToFolder :: Setting (->) s t [a] [a] -> [a] -> s -> t
addToFolder lens what = over lens (++ what)

filteredByName
  :: (Choice p, Applicative f) =>
     FileName -> Optic' p f (GDir) (GDir)
filteredByName name = filtered ((== name) . (^. _name))

projectRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
projectRoot = _dirTree._contents

addToProject :: [GDir] -> GState -> GState
addToProject = addToFolder projectRoot

viewProjectRoot :: GState -> [GDir]
viewProjectRoot = view projectRoot

gshellRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
gshellRoot = projectRoot.traverse.filteredByName gshellFolderName._contents

addToGshell :: [GDir] -> GState -> GState
addToGshell = addToFolder gshellRoot

viewGshellRoot :: GState -> [GDir]
viewGshellRoot = view gshellRoot

commitsRoot :: Control.Applicative.Applicative f =>
     ([GDir] -> f [GDir])
     -> GState -> f (GState)
commitsRoot = gshellRoot.traverse.filteredByName commitsFolderName._contents

addToCommits :: [GDir] -> GState -> GState
addToCommits = addToFolder commitsRoot

viewCommitsRoot :: GState -> [GDir]
viewCommitsRoot = view commitsRoot

generateState :: FilePath -> IO GState
generateState = readDirectoryWithL T.readFile

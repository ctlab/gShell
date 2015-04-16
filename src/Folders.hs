module Folders ( gshellFolderName
               , gf
               , commitsFolderName
               , cf
               , revFolderName
               , rf
               , mountFolderName
               , mf
               , workFolderName
               , wf
               , generateHash
               , generateId
               ) where

import           Control.Applicative
import           Data.Time.Clock.POSIX
import           System.FilePath
import           System.Random

gshellFolderName = ".gshell"
gf path = path </> gshellFolderName

commitsFolderName = "commits"
cf path = path </> gshellFolderName </> commitsFolderName

revFolderName = "rev"
rf path = path </> gshellFolderName </> commitsFolderName </> revFolderName

mountFolderName = "toMount"
mf path revName = path </> gshellFolderName </> commitsFolderName </> revName </> mountFolderName

workFolderName = "work-id"
wf path = path </> workFolderName

generateHash :: IO FilePath
generateHash = do
    g <- newStdGen
    time <- show <$> getPOSIXTime
    let a = take 5 $ (randomRs ('a', 'z') g)
    let b = take 5 $ (randomRs ('0', '9') g)
    let hash = concat $ zipWith (\a b -> a:[b]) a b
    return $ "-" ++ time ++ "-" ++ hash

generateId :: IO FilePath
generateId = do
    g <- newStdGen
    let c = (randomRs ('0', '9') g)
    let a = take 2 $ c
    let b = take 2 $ drop 2 c
    return $ a ++ b

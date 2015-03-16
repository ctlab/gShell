module Folders where

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

workFolderName = "work-id"
wf path = path </> workFolderName

generateHash :: IO FilePath
generateHash = do
    g <- getStdGen
    time <- show <$> getPOSIXTime
    let a = take 5 $ (randomRs ('a', 'z') g)
    let b = take 5 $ (randomRs ('0', '9') g)
    let hash = concat $ zipWith (\a b -> a:[b]) a b
    return $ "-" ++ time ++ "-" ++ hash

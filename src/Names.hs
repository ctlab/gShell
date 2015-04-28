module Names ( gshellDirName
             , gshellDir
             , commitsDirName
             , commitsDir
             , commitFileName
             , commitFile
             , revDirName
             , revDir
             , mountDirName
             , mountDir
             , workDirName
             , workDir
             , workHelperFileName
             , workHelperFile
             , generateHash
             , generateId
             , gshellInited
             ) where

import           Control.Applicative
import           Data.Time.Clock.POSIX
import           System.FilePath
import           System.Random

gshellDirName = ".gshell"
gshellDir path = path </> gshellDirName

commitsDirName = "commits"
commitsDir path = path </> gshellDirName </> commitsDirName

commitFileName = "commit"
commitFile path revName = path </> gshellDirName </> commitsDirName </> revName </> mountDirName

revDirName = "rev"
revDir path = path </> gshellDirName </> commitsDirName </> revDirName

mountDirName = "toMount"
mountDir path revName = path </> gshellDirName </> commitsDirName </> revName </> mountDirName

workDirName = "work-id"
workDir path = path </> workDirName

workHelperFileName = "work-information"
workHelperFile path workId = path </> gshellDirName </> (workDirName ++ workId) </> workHelperFileName

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

gshellInited :: Bool -> String
gshellInited a = "gshell is " ++ (if a then "already" else "not") ++ " inited"
module Folders where
import System.FilePath

gshellFolderName = ".gshell"
gf path = path </> gshellFolderName

commitsFolderName = "commits"
cf path = path </> gshellFolderName </> commitsFolderName

revFolderName = "rev"
rf path = path </> gshellFolderName </> commitsFolderName </> revFolderName

workFolderName = "work-id"
wf path = path </> workFolderName

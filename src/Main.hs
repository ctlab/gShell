module Main where

import           Gshell.Command
import           Gshell.Run
import           Gshell.State

import           Data.String         (fromString)

import           System.Directory
import           System.Environment  (getArgs)

import           Options.Applicative

main :: IO ()
main = do
    currentPath <- getCurrentDirectory
    options' <- execParser
        $ parseOptions currentPath `withInfo` "Interact with gshell"
    options <- case options' of
        (Options (Init path) a) -> checkAndAdjust path Init
        (Options (Clear path) a) -> checkAndAdjust path Clear
        (Options (Enter path) a) -> checkAndAdjust path Enter
        _ -> return options'
    res <- run options
    case res of
         Right r -> case r of
            ResultInfo info -> mapM_ putStrLn info
            ResultPath path -> putStrLn path
            ResultCommand command -> putStrLn $ "Success: " ++ show command 
         _ -> print res

checkAndAdjust :: FilePath -> (FilePath -> Command) -> IO Options
checkAndAdjust path f = do
    exist <- doesDirectoryExist path
    truePath <- if exist 
            then canonicalizePath path 
            else error "Create project's folder first"
    return $ Options (f truePath) truePath

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: FilePath -> Parser Options
parseOptions currentPath = Options <$> parseCommand <*> parsePath currentPath

parseCommand :: Parser Command
parseCommand = subparser $
    command "init"     (parseInit `withInfo` "Init gshell") <>
    command "enter"    (parseEnter `withInfo` "Enter project with gshell inited") <>
    command "clear"    (parseClear `withInfo` "Unmount workspaces and remove data") <>
    command "push"     (parsePush `withInfo` "Push changes to master") <>
    command "pull"     (parsePull `withInfo` "Get and apply changes from master") <>
    command "commit"   (parseCommit `withInfo` "Commit current action with message") <>
    command "checkout" (parseEnterRevision `withInfo` "Go to a specific revision") <>
    command "rollback" (parseRollback `withInfo` "Undo changes") <>
    command "enterRev" (parseEnterRevision `withInfo` "Enter project with gshell inited at a specific revision") <>
    command "log"      (parseLog `withInfo` "Show log for current workspace") <>
    command "graph"    (parseGetGraph `withInfo` "Show graph from current workspace") <>
    command "off"      (option disabled idm `withInfo` "Temporary disable gshell") <>
    command "on"      (option disabled idm `withInfo` "Temporary enable gshell")

parsePath :: FilePath -> Parser FilePath
parsePath currentPath = strOption
    $ short 'p' <> long "path"
    <> metavar "WORKING-PATH"
    <> help "Project's path where gshell is inited"
    <> value currentPath

parseInit :: Parser Command
parseInit = Init <$> argument str (metavar "PROJECT-FOLDER")

parseEnter :: Parser Command
parseEnter = Enter <$> argument str (metavar "PROJECT-FOLDER")

parseClear :: Parser Command
parseClear = Clear <$> argument str (metavar "PROJECT-FOLDER")

parsePush :: Parser Command
parsePush = pure Push

parsePull :: Parser Command
parsePull = pure Pull

parseLog :: Parser Command
parseLog = pure Log

parseGetGraph :: Parser Command
parseGetGraph = pure GetGraph

parseCommit :: Parser Command
parseCommit = Commit <$> argument str (metavar "COMMIT-MESSAGE")

parseRollback :: Parser Command
parseRollback = pure Rollback

parseEnterRevision :: Parser Command
parseEnterRevision = EnterRevision <$> argument str (metavar "REVISION-ID")

parseCheckout :: Parser Command
parseCheckout = parseEnterRevision

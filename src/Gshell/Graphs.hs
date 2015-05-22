module Gshell.Graphs ( printGraph
                     ) where

import           Control.Applicative
import           Data.Graph.Inductive              hiding (mkEdges, mkNodes)
import           Data.GraphViz
import           Data.GraphViz.Attributes
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Printing
import           Data.Text.Lazy                    (Text, pack, unpack)

import           Control.Lens
import           Data.List
import           Gshell.Names
import           Gshell.State

import           Control.Monad
import qualified Data.Map                          as M
import           System.Directory
import           System.Directory.Tree

command :: String -> (Text, Shape)
command = flip (,) Circle . pack

content :: String -> (Text, Shape)
content = flip (,) BoxShape . pack

printGraph :: GState -> IO ()
printGraph state = do
    graph <- buildGraph state
    putStrLn $ unpack $ renderDot $ toDot $ graphToDot graphParams graph

buildGraph :: GState -> IO (Gr (Text, Shape) StyleName)
buildGraph state = do
    let revisions = sortBy (cmpRevs state)
            $ filter (\revName -> not $ null $ state ^. revCommit revName)
            $ state ^.. commitsRoot.traverse._name
    nodes <- mkNodes state revisions
    let edges = mkEdges state revisions
    return $ mkGraph nodes edges

mkNodes :: GState -> [FilePath] -> IO ([(Int, (Text, Shape))])
mkNodes state revisions = do
    contents <- mapM (
        liftM (content . intercalate "\n" . filter (\a -> a /= "." && a /= ".."))
        . getDirectoryContents
        . mountDir (projectPath state))
        revisions
    let commands = map (\a -> command $ state ^. revCommit a) revisions
    return $ zip [0..] $ concat $ transpose [commands, contents]

mkEdges :: GState -> [FilePath] -> [(Int, Int, StyleName)]
mkEdges state revisions' = concatMap (filter (\(a, b, _) -> a /= b + 1) . join . ($ revisions)) [parRels, commandRels, openRels]
    where
        revisions = flip zip [0..] revisions'
        commandRels revisions = (map (\(_, num) -> [(getCommandByNum num, getContentByNum num, Solid)]) revisions)
        parRels = (connect pars Dotted =<<)
        openRels = (connect opens Solid =<<)
        pars = (^. parentsRevs) . read . (state ^.) . parents
        opens = M.keys . flip getReadLog state
        connect suppers lineStyle (revName, num) = map (\p -> [(getContentByNum $ getNum p, getCommandByNum num, lineStyle)]) $ suppers revName
        getNum = ($ M.fromList revisions) . M.findWithDefault (-1)

getCommandByNum n = n * 2
getContentByNum n = n * 2 + 1

graphParams  :: GraphvizParams t (Text, Shape) StyleName () (Text, Shape)
graphParams = nonClusteredParams { globalAttributes = attrs, fmtNode = fn, fmtEdge = fe }
    where
        attrs = [ GraphAttrs
               [ RankDir FromLeft
               , BgColor [toWColor Transparent]
               ]
             , NodeAttrs [ Shape BoxShape
                         , FillColor [toWColor White]
                         , Style [SItem Filled []]
                         ]
             ]
        fn (n, (l, s)) = [(Label . StrLabel) l, Shape s]
        fe (f, t, s) = [Style $ [SItem s []]]

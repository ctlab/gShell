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
    nodes <- mkNodes state
    let edges = mkEdges state
    return $ mkGraph nodes edges

mkNodes :: GState -> IO ([(Int, (Text, Shape))])
mkNodes state = do
    contents <- mapM (liftM (content . intercalate "\n" . filter (\a -> a /= "." && a /= "..")) . getDirectoryContents . mountDir (projectPath state)) revisions
    let commands = map (\a -> command $ state ^. revCommit a) revisions
    return $ zip [0..] $ concat $ transpose [commands, contents]
    where
        revisions = sortBy (cmpRevs state) $ state ^.. commitsRoot.traverse._name

mkEdges :: GState -> [(Int, Int, StyleName)]
mkEdges state = result
    where
        revisions = flip zip [0..] $ sortBy (cmpRevs state) $ state ^.. commitsRoot.traverse._name
        getCommandByNum n = n * 2
        getContentByNum n = n * 2 + 1
        commandRels = zipWith (\a b -> [(a, b, Solid)]) (map (getCommandByNum . snd) revisions) (map (getContentByNum . snd) revisions)
        parRels = concatMap (connect (pars) Dotted) revisions
        pars revName = (read $ state ^. parents revName) ^. parentsRevs
        openRels = concatMap (connect (opens) Solid) revisions
        opens revName = M.keys $ getReadLog revName state
        connect suppers lineStyle (revName, num) = map (\p -> [(getContentByNum $ getNum p, getCommandByNum num, lineStyle)]) $ suppers revName
        getNum p = M.findWithDefault (-1) p $ M.fromList revisions
        result = concat $ map join [parRels, commandRels, openRels]

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

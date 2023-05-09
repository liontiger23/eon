{-# LANGUAGE TupleSections #-}

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import qualified Data.GraphViz.Types.Monadic as GV
import Data.GraphViz.Types.Monadic ((-->), cluster, graphAttrs, nodeAttrs, digraph)
import Data.GraphViz.Attributes
import Data.GraphViz.Printing
import Text.XML.Light

import Dependencies
import Ergonomics (unique)

main = do
  args <- getArgs
  case parseCommand args of
    Command f args -> f args
    Error msg -> putStrLn $ "Error - " <> msg

parseCommand :: [String] -> Action
parseCommand [] = Error "no command given"
parseCommand [c] = Error $ "missing argument(s) for command: " <> c
parseCommand (c:args) =
  case c of
    "depends-on" -> Command dependsOn args
    "dot-deps" -> Command dotDeps args
    "dot-rev-deps" -> Command dotRevDeps args
    "dot-deps-and-rev-deps" -> Command dotDepsAndRevDeps args
    _ -> Error $ "unknown command: " <> c

data Action
  = Command ([String] -> IO ()) [String]
  | Error String

dependsOn :: [String] -> IO ()
dependsOn pkgs = collectRevDeps pkgs >>= putStr . unlines . unique

dotDeps :: [String] -> IO ()
dotDeps (xml : root : _) = do
  file <- B.readFile xml
  putStrLn $ printDeps $ depsClosure (T.pack root) $ parseXMLDeps file

dotRevDeps :: [String] -> IO ()
dotRevDeps (xml : root : _) = do
  file <- B.readFile xml
  putStrLn $ printDeps $ reverseDeps $ depsClosure (T.pack root) $ reverseDeps $ parseXMLDeps file

dotDepsAndRevDeps :: [String] -> IO ()
dotDepsAndRevDeps (xml : depRoot : revDepRoot : _) = do
  file <- B.readFile xml
  putStrLn $ printDeps $
    depsClosure (T.pack revDepRoot) $ reverseDeps $
    depsClosure (T.pack depRoot) $ reverseDeps $
    parseXMLDeps file


printDeps deps = T.unpack $ renderDot $ toDot (DotGraph {
    strictGraph = True,
    directedGraph = True,
    graphID = Just $ Str $ T.pack "G",
    graphStatements = DotStmts {
      attrStmts = [],
      subGraphs = [],
      nodeStmts = [],
      edgeStmts = do
        (package, deps) <- Map.assocs deps
        dep <- deps
        [DotEdge package dep []]
    }
  } :: DotGraph Text)

type Deps = Map Text [Text]

depsClosure :: Text -> Deps -> Deps
depsClosure init deps = collectClosure Map.empty [init]
  where
    collectClosure :: Deps -> [Text] -> Deps
    collectClosure res [] = res
    collectClosure res (x:xs)
      | Map.member x res ||
        Map.notMember x deps = collectClosure res xs
      | otherwise = collectClosure (Map.insert x ys res) $ xs ++ ys
      where
        ys = deps ! x

reverseDeps :: Deps -> Deps
reverseDeps = Map.foldrWithKey collectRevDeps Map.empty
  where
    collectRevDeps :: Text -> [Text] -> Deps -> Deps
    collectRevDeps p deps m = foldr (\d -> Map.insertWith (++) d [p]) m deps


parseXMLDeps :: B.ByteString -> Deps
parseXMLDeps file =
  Map.fromListWith (++) $ concat $ mapM extractDeps packages
  where
    contents = parseXML file
    packages = concatMap (findChildren $ unqual "Package") $ onlyElems contents
    extractDeps :: Element -> Maybe (Text, [Text])
    extractDeps elem = fmap (, deps) maybeName
      where
        maybeName = normalizePackage . T.pack . strContent <$> findChild (unqual "Name") elem
        deps = concatMap (map (normalizePackage . T.pack . strContent) . onlyElems . elContent) $ findChild (unqual "RuntimeDependencies") elem

normalizePackage :: Text -> Text
normalizePackage name =
  case T.breakOnEnd (T.pack "-") name of
    (prefix, suffix)
      | suffix == T.pack "dbginfo" ||
        suffix == T.pack "devel" -> T.init prefix
      | otherwise -> name

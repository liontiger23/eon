import qualified Data.Text.Lazy as T
import System.Environment (getArgs)
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes
import Data.GraphViz.Printing

import Dependencies
import Ergonomics (unique)

data Action
  = Command ([String] -> IO ()) [String]
  | Error String

dependsOn :: [String] -> IO ()
dependsOn pkgs = collectRevDeps pkgs >>= putStr . unlines . unique

dependencyGraph :: [String] -> IO ()
dependencyGraph _ = putStrLn $ T.unpack $ renderDot $ toDot $ digraph (Str $ T.pack "G") $ do

   cluster (Num $ Int 0) $ do
       graphAttrs [style filled, color LightGray]
       nodeAttrs [style filled, color White]
       "a0" --> "a1"
       "a1" --> "a2"
       "a2" --> "a3"
       graphAttrs [textLabel $ T.pack "process #1"]

   cluster (Num $ Int 1) $ do
       nodeAttrs [style filled]
       "b0" --> "b1"
       "b1" --> "b2"
       "b2" --> "b3"
       graphAttrs [textLabel $ T.pack "process #2", color Blue]

   "start" --> "a0"
   "start" --> "b0"
   "a1" --> "b3"
   "b2" --> "a3"
   "a3" --> "end"
   "b3" --> "end"

   node "start" [shape MDiamond]
   node "end" [shape MSquare]

parseCommand :: [String] -> Action
parseCommand [] = Error "no command given"
parseCommand [c] = Error $ "missing argument(s) for command: " <> c
parseCommand (c:args) =
  case c of
    "depends-on" -> Command dependsOn args
    "graph" -> Command dependencyGraph args
    _ -> Error $ "unknown command: " <> c

main = do
  args <- getArgs
  case parseCommand args of
    Command f args -> f args
    Error msg -> putStrLn $ "Error - " <> msg

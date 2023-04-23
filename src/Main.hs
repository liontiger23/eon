import System.Environment (getArgs)

import Dependencies
import Ergonomics (unique)

data Action
  = Command ([String] -> IO ()) [String]
  | Error String

dependsOn :: [String] -> IO ()
dependsOn pkgs = collectRevDeps pkgs >>= putStr . unlines . unique

parseCommand :: [String] -> Action
parseCommand [] = Error "no command given"
parseCommand [c] = Error $ "missing argument(s) for command: " <> c
parseCommand (c:args) =
  case c of
    "depends-on" -> Command dependsOn args
    _ -> Error $ "unknown command: " <> c

main = do
  args <- getArgs
  case parseCommand args of
    Command f args -> f args
    Error msg -> putStrLn $ "Error - " <> msg

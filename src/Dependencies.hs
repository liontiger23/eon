module Dependencies
  ( collectRevDeps
  ) where

import Data.List.Split (splitOn)
import System.Process (readProcess)
import Text.Regex.PCRE ((=~))

import Ergonomics

eopkgInfo :: [String] -> IO [String]
eopkgInfo pkgs = do
  info <- readProcess "eopkg" ("info" : pkgs) []
  return $ splitOn "\n\n" info

revDepsOf :: [String] -> IO [String]
revDepsOf pkgs = do
  infos <- eopkgInfo pkgs
  return $
    infos
    |> filter isRemote
    |> map extractRevDeps
    |> concat
    |> filter (not . isSubpackage)
  where
    isRemote = (`contains` "Package found")
    isSubpackage pkg = (pkg `endsWith`) `any` ["dbginfo", "devel"]
    extractRevDeps = words . (=~ "[^:]+\\z")

collectRevDeps :: [String] -> IO [String]
collectRevDeps start = do
  revDeps <- revDepsOf start
  if null revDeps
    then return start
    else do
      tree <- collectRevDeps revDeps
      return (start ++ revDeps ++ tree)

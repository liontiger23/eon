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
    |> filter is_remote
    |> map extract_revdeps
    |> concat
    |> filter (not . is_subpackage)
  where
    is_remote = (`contains` "Package found")
    is_subpackage pkg = (pkg `endsWith`) `any` ["dbginfo", "devel"]
    extract_revdeps = words . (=~ "[^:]+\\z")

collectRevDeps :: [String] -> IO [String]
collectRevDeps start = do
  revdeps <- revDepsOf start
  if null revdeps
    then return start
    else do
      tree <- collectRevDeps revdeps
      return (start ++ revdeps ++ tree)

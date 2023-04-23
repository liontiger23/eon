module Ergonomics where

import Data.Function ((&))
import Data.List (isInfixOf, isSuffixOf, nub)

(|>) = (&)

contains :: Eq a => [a] -> [a] -> Bool
contains = flip isInfixOf

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = flip isSuffixOf

unique :: Eq a => [a] -> [a]
unique = nub

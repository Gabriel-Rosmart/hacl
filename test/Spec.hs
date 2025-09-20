module Main (main) where

import Test.Hspec (hspec)
import qualified ParsingSpec as PS
import qualified ResolverSpec as RS

main :: IO ()
main = hspec (PS.spec >> RS.spec) 

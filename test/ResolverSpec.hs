{-# LANGUAGE OverloadedStrings #-}

module ResolverSpec where

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Hacl.Parser
import Data.Hacl.Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Map as M

spec :: Spec

spec = do
  describe "Hacl Resolver Dummy" $ do 
    it "Parses true" $ parse hbool "" "true" `shouldParse` (HaclBool True)


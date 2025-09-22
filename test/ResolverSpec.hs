{-# LANGUAGE OverloadedStrings #-}

module ResolverSpec where

import Test.Hspec
import Data.Hacl.Types
import Data.Hacl.Resolver
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Hacl.Error

spec :: Spec

spec = do
  describe "IO Hacl" $ do 
    it "Parses file with no imports" $
      (haclFromFile "test/assets/single.hacl") `shouldReturn` (Right $ HaclObject $ M.fromList [(T.pack "single", HaclBool True)])
    
    it "Parses file with imports" $
      (haclFromFile "test/assets/wimport.hacl")
        `shouldReturn` (Right $ HaclObject $ M.fromList [(T.pack "import", HaclObject $ M.fromList [(T.pack "single", HaclBool True)])])
        
  describe "Failing IO Hacl" $ do 
    it "Cannot import unexisting file" $
      (haclFromFile "test/assets/unexisting.hacl") 
        `shouldReturn` (Left (ResourceNotFound "does_not_exist.hacl"))
    
    it "Prevents recursive import to same file" $  
      (haclFromFile "test/assets/recursive.hacl") 
        `shouldReturn` (Left (RecursiveImport "test/assets/recursive.hacl" "test/assets/recursive.hacl"))
    
    it "Prevents recursive imports between files" $
      (haclFromFile "test/assets/recursive_between1.hacl") 
        `shouldReturn` (Left (RecursiveImport "test/assets/recursive_between2.hacl" "test/assets/recursive_between1.hacl")) 

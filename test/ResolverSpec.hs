{-# LANGUAGE OverloadedStrings #-}

module ResolverSpec where

import Test.Hspec
import Data.Hacl.Types
import Data.Hacl.Resolver
import qualified Data.Text as T
import qualified Data.Map as M

spec :: Spec

spec = do
  describe "IO Hacl" $ do 
    it "Parses file with no imports" $
      (haclFromFile "test/assets/single.hacl") `shouldReturn` (HaclObject $ M.fromList [(T.pack "single", HaclBool True)])
    
    it "Parses file with imports" $
      (haclFromFile "test/assets/wimport.hacl")
        `shouldReturn` (HaclObject $ M.fromList [(T.pack "import", HaclObject $ M.fromList [(T.pack "single", HaclBool True)])])
        
  describe "Failing IO Hacl" $ do 
    it "Cannot import unexisting file" $
      (haclFromFile "test/assets/unexisting.hacl") `shouldThrow` anyException
    
    it "Prevents recursive import to same file" $  
      (haclFromFile "test/assets/recursive.hacl") `shouldThrow` anyErrorCall
    
    it "Prevents recursive imports between files" $ (haclFromFile "test/assets/recursive_between1.hacl") `shouldThrow` anyErrorCall 

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec
import Hacl
import HaclType
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Map as M


main :: IO ()
main = hspec $ do
  describe "Hacl Single Token Parse" $ do 
    it "Parses true" $ parse hbool "" "true" `shouldParse` (HaclBool True)
    it "Parses false" $ parse hbool "" "false" `shouldParse` (HaclBool False)
    it "Parses nothing keyword" $ parse hnothing "" "nothing" `shouldParse` (HaclNothing)
  
  describe "Hacl Number Parse" $ do 
    it "Parses integer" $ parse hnumber "" "7" `shouldParse` (HaclNumber (HaclInteger 7))
    it "Parses negative integer" $ parse hnumber "" "-7" `shouldParse` (HaclNumber (HaclInteger (-7)))
    it "Parses integer with exponent" $ parse hnumber "" "7e2" `shouldParse` (HaclNumber (HaclInteger 700))
    it "Parses integer with negative exponent" $ parse hnumber "" "7e-2" `shouldParse` (HaclNumber (HaclDouble 0.07))
    it "Parses negative integer with exponent" $ parse hnumber "" "-7e2" `shouldParse` (HaclNumber (HaclInteger (-700)))
    it "Parses negative integer with negative exponent" $ parse hnumber "" "-7e-2" `shouldParse` (HaclNumber (HaclDouble (-0.07)))
    it "Parses double" $ parse hnumber "" "3.14" `shouldParse` (HaclNumber (HaclDouble 3.14))
    it "Parses negative double" $ parse hnumber "" "-3.14" `shouldParse` (HaclNumber (HaclDouble (-3.14)))
    it "Parses double with exponent" $ parse hnumber "" "3.14e2" `shouldParse` (HaclNumber (HaclInteger 314))
    it "Parses double with negative exponent" $ parse hnumber "" "3.14e-2" `shouldParse` (HaclNumber (HaclDouble 0.0314))
    it "Parses negative double with exponent" $ parse hnumber "" "-3.14e2" `shouldParse` (HaclNumber (HaclInteger (-314)))
    it "Parses negative double with negative exponent" $ parse hnumber "" "-3.14e-2" `shouldParse` (HaclNumber (HaclDouble (-0.0314)))

  describe "Hacl String Parse" $ do 
    it "Parses no (escape | whitespace) string" $ parse hstring "" "\"HaCL\"" `shouldParse` (HaclText (T.pack "HaCL"))
    it "Parses no escape string" $ parse hstring "" "\"HaCL Lang\"" `shouldParse` (HaclText (T.pack "HaCL Lang"))
    it "Parses escape string" $ parse hstring "" "\"HaCL\n\t\b\r\"" `shouldParse` (HaclText (T.pack "HaCL\n\t\b\r"))

  describe "Hacl Array Parse" $ do 
    it "Parses uncommented array" $ parse harray "" "[true, nothing, 5]"
      `shouldParse` (HaclArray (NE.fromList [HaclBool True, HaclNothing, HaclNumber (HaclInteger 5)]))
    
    it "Parses commented array" $ parse harray "" "[true, // First\n nothing, // Second \n 5]"
      `shouldParse` (HaclArray (NE.fromList [HaclBool True, HaclNothing, HaclNumber (HaclInteger 5)]))

  describe "Hacl Object Parse" $ do 
    it "Parses uncommented object" $ parse hobject "" "{\"keepalive\" : false}"
      `shouldParse` (HaclObject (M.fromList [(T.pack "keepalive", HaclBool False)]))
    it "Parses commented object" $ parse hobject "" "{\"keepalive\" : false, // First \n\"ttl\" : 60}"
      `shouldParse` (HaclObject (M.fromList [(T.pack "keepalive", HaclBool False), (T.pack "ttl", HaclNumber (HaclInteger 60))]))

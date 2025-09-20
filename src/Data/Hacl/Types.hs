module Data.Hacl.Types where

import qualified Data.Text as T
import Data.Scientific
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

data Hacl = HaclText T.Text |
            HaclBool Bool |
            HaclNothing |
            HaclNumber HaclNumberType |
            HaclArray (NE.NonEmpty Hacl) |
            HaclObject (M.Map T.Text Hacl) |
            HaclImport T.Text
            deriving Eq

data HaclNumberType = HaclInteger Int | HaclDouble Double deriving Eq

castHaclNumber :: Scientific -> HaclNumberType
castHaclNumber n | (isFloating n) = HaclDouble (toRealFloat n)
                 | otherwise = HaclInteger (fromMaybe 0 (toBoundedInteger n))

-- Pretty Printing --

instance Show HaclNumberType where
  show (HaclDouble d) = show d 
  show (HaclInteger i) = show i

instance Show Hacl where
  show h = ppHacl False 0 h

ppHacl :: Bool -> Int -> Hacl -> String 

ppHacl isObjectValue n (HaclObject o) = (ident ++ "{\n")
  ++
    (
      intercalate ", \n" (map (\p -> ident_val ++ (quote $ T.unpack (fst p)) ++ " : " ++ (ppHacl True (n + 1) $ snd p)) (M.toList o))
    )
  ++ ("\n" ++ ((concat $ replicate (n * 4) " ")) ++ "}")

  where ident = identKV isObjectValue n
        ident_val = (concat $ replicate ((n + 1) * 4) " ")

ppHacl isObjectValue n (HaclArray a) = (identf ++ "[\n")
  ++
    (
      intercalate ", \n" (NE.toList $ NE.map (ppHacl (not isObjectValue) $ n + 1) a)
    )
  ++ ("\n" ++ identl ++ "]")

  where identf = identKV isObjectValue n
        identl = identKV (not isObjectValue) n


ppHacl b n (HaclNumber h) = (identKV b n) ++ (show h)
ppHacl b n (HaclText t) = (identKV b n) ++ (quote $ T.unpack t)
ppHacl x n (HaclBool b) = (identKV x n) ++ (show b)
ppHacl b n (HaclNothing) = (identKV b n)  ++ "nothing"
ppHacl b n (HaclImport i) = (identKV b n) ++ "import " ++ (quote $ T.unpack i)

-- If is an object value do not ident, otherwise ident
identKV :: Bool -> Int -> String
identKV False indentlvl = (concat $ replicate (indentlvl * 4) " ")
identKV True _ = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

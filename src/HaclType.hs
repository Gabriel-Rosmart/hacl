module HaclType where

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

data HaclNumberType = HaclInteger Int | HaclDouble Double

castHaclNumber :: Scientific -> HaclNumberType
castHaclNumber n | (isFloating n) = HaclDouble (toRealFloat n)
                 | otherwise = HaclInteger (fromMaybe 0 (toBoundedInteger n))


instance Show HaclNumberType where
  show (HaclDouble d) = show d 
  show (HaclInteger i) = show i

instance Show Hacl where
  show h = ppHacl 0 h

ppHacl :: Int -> Hacl -> String 

ppHacl n (HaclObject o) = (ident ++ "{\n")
  ++
    (
      intercalate ", \n" (map (\p -> ident_val ++ (quote $ T.unpack (fst p)) ++ " : " ++ (hppHacl (n + 1) $ snd p)) (M.toList o))
    )
  ++ ("\n" ++ ident ++ "}")

  where ident = concat $ replicate (n * 4) " "
        ident_val = concat $ replicate ((n + 1) * 4) " "

ppHacl n (HaclArray a) = (ident ++ "[\n")
  ++
    (
      intercalate ", \n" (NE.toList $ NE.map (ppHacl $ n + 1) a)
    )
  ++ ("\n" ++ ident ++ "]")

  where ident = concat $ replicate (n * 4) " "


ppHacl n (HaclNumber h) = (concat $ replicate (n * 4) " ") ++ (show h)
ppHacl n (HaclText t) = (concat $ replicate (n * 4) " ") ++ (quote $ T.unpack t)
ppHacl n (HaclImport i) = (concat $ replicate (n * 4) " ") ++ "import " ++ (quote $ T.unpack i)
ppHacl n (HaclBool b) = (concat $ replicate (n * 4) " ") ++ (show b)
ppHacl n (HaclNothing) = (concat $ replicate (n * 4) " ") ++ "nothing"



hppHacl :: Int -> Hacl -> String 

hppHacl n (HaclObject o) = "{\n"
  ++
    (
      intercalate ", \n" (map (\p -> ident_val ++ (quote $ T.unpack (fst p)) ++ " : " ++ (ppHacl (n + 1) $ snd p)) (M.toList o))
    )
  ++ ("\n" ++ ident ++ "}")

  where ident = concat $ replicate (n * 4) " "
        ident_val = concat $ replicate ((n + 1) * 4) " "

hppHacl n (HaclArray a) = "[\n"
  ++
    (
      intercalate ", \n" (NE.toList $ NE.map (ppHacl $ n + 1) a)
    )
  ++ ("\n" ++ ident ++ "]")

  where ident = concat $ replicate (n * 4) " "


hppHacl _ (HaclNumber h) = (show h)
hppHacl _ (HaclText t) = (quote $ T.unpack t)
hppHacl _ (HaclImport i) = "import " ++ (quote $ T.unpack i)
hppHacl _ (HaclBool b) = (show b)
hppHacl _ (HaclNothing) = "nothing"

identKV :: Bool -> Int -> String
identKV True indentlvl = (concat $ replicate (indentlvl * 4) " ")
identKV False _ = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

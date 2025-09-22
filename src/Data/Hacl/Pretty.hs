module Data.Hacl.Pretty (
                  pretty,
                  prettyError,
                  prettyM,
                  prettyL, 
                  prettyML) 
                  where 
 
import Data.Hacl.Types
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Hacl.Error
import Text.Megaparsec.Error(errorBundlePretty)
import Data.Scientific

-- Exported function

pretty :: Hacl -> String
pretty h = ppHacl False 0 h

prettyWithLvl :: Int -> Hacl -> String
prettyWithLvl lvl h = ppHacl True lvl h

prettyM :: Maybe Hacl -> String
prettyM Nothing = "Nothing"
prettyM (Just h) = "Just " ++ pretty h

prettyL :: [Hacl] -> String
prettyL hl = "[\n" ++ (intercalate ",\n" (map (prettyWithLvl 1) hl)) ++ "\n]"

prettyML :: Maybe [Hacl] -> String
prettyML Nothing = "Nothing"
prettyML (Just hl) = "Just " ++ prettyL hl

-- Pretty Print Errors

prettyError :: HaclError -> String
prettyError (RecursiveImport c cl) = "Forbidden recursive import, file " ++ c ++ " tried to import " ++ cl
prettyError (ResourceNotFound r) = "Resource " ++ r ++ " not found"
prettyError (MegaparsecParseError e) = errorBundlePretty e

-- Inner Pretty Print functions

ppHacl :: Bool -> Int -> Hacl -> String 

ppHacl shouldBeIndented ilvl (HaclObject o) = (indent_open ++ "{\n")
  ++
    (
      intercalate ", \n" (map (\p -> indent_value ++ (quote $ T.unpack (fst p)) ++ " : " ++ (ppHacl False (ilvl + 1) $ snd p)) (M.toList o))
    )
  ++ ("\n" ++ indent_closing ++ "}")

  where indent_open = getIndentation shouldBeIndented ilvl
        indent_closing = concat $ replicate (ilvl * 4) " "
        indent_value = concat $ replicate ((ilvl + 1) * 4) " "

ppHacl shouldBeIndented ilvl (HaclArray a) = (indent_open ++ "[\n")
  ++
    (
      intercalate ", \n" (NE.toList $ NE.map (ppHacl True $ ilvl + 1) a)
    )
  ++ ("\n" ++ indent_closing ++ "]")

  where indent_open = getIndentation shouldBeIndented ilvl
        indent_closing = concat $ replicate (ilvl * 4) " "


ppHacl shouldBeIndented ilvl (HaclNumber n) = (getIndentation shouldBeIndented ilvl) ++ (ppHaclNumber n)
ppHacl shouldBeIndented ilvl (HaclText t) = (getIndentation shouldBeIndented ilvl) ++ (quote $ T.unpack t)
ppHacl shouldBeIndented ilvl (HaclBool b) = (getIndentation shouldBeIndented ilvl) ++ (show b)
ppHacl shouldBeIndented ilvl (HaclNothing) = (getIndentation shouldBeIndented ilvl) ++ "nothing"
ppHacl shouldBeIndented ilvl (HaclImport i) = (getIndentation shouldBeIndented ilvl) ++ "import " ++ (quote $ T.unpack i)

ppHaclNumber :: Scientific -> String
ppHaclNumber n = if isInteger n then (formatScientific Generic (Just 0) n) else (formatScientific Generic Nothing n) 

getIndentation :: Bool -> Int -> String
getIndentation True ilvl = (concat $ replicate (ilvl * 4) " ")
getIndentation False _ = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

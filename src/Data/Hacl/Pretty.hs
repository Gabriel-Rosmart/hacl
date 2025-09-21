module Data.Hacl.Pretty (
                  pretty,
                  prettyM,
                  prettyL, 
                  prettyML) 
                  where 
 
import Data.Hacl.Types
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

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


ppHacl shouldBeIndented ilvl (HaclNumber h) = (getIndentation shouldBeIndented ilvl) ++ (ppHaclNumber h)
ppHacl shouldBeIndented ilvl (HaclText t) = (getIndentation shouldBeIndented ilvl) ++ (quote $ T.unpack t)
ppHacl shouldBeIndented ilvl (HaclBool b) = (getIndentation shouldBeIndented ilvl) ++ (show b)
ppHacl shouldBeIndented ilvl (HaclNothing) = (getIndentation shouldBeIndented ilvl) ++ "nothing"
ppHacl shouldBeIndented ilvl (HaclImport i) = (getIndentation shouldBeIndented ilvl) ++ "import " ++ (quote $ T.unpack i)

ppHaclNumber :: HaclNumberType -> String
ppHaclNumber (HaclDouble d) = show d 
ppHaclNumber (HaclInteger i) = show i

getIndentation :: Bool -> Int -> String
getIndentation True ilvl = (concat $ replicate (ilvl * 4) " ")
getIndentation False _ = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

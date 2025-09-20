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

prettyM :: Maybe Hacl -> String
prettyM Nothing = "Nothing"
prettyM (Just h) = "Just " ++ pretty h

prettyL :: [Hacl] -> String
prettyL hl = "[\n" ++ (intercalate ",\n" (map pretty hl)) ++ "\n]"

prettyML :: Maybe [Hacl] -> String
prettyML Nothing = "Nothing"
prettyML (Just hl) = "Just " ++ prettyL hl

-- Inner Pretty Print functions

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


ppHacl b n (HaclNumber h) = (identKV b n) ++ (ppHaclNumber h)
ppHacl b n (HaclText t) = (identKV b n) ++ (quote $ T.unpack t)
ppHacl x n (HaclBool b) = (identKV x n) ++ (show b)
ppHacl b n (HaclNothing) = (identKV b n)  ++ "nothing"
ppHacl b n (HaclImport i) = (identKV b n) ++ "import " ++ (quote $ T.unpack i)

ppHaclNumber :: HaclNumberType -> String
ppHaclNumber (HaclDouble d) = show d 
ppHaclNumber (HaclInteger i) = show i

-- If is an object value do not ident, otherwise ident
identKV :: Bool -> Int -> String
identKV False indentlvl = (concat $ replicate (indentlvl * 4) " ")
identKV True _ = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

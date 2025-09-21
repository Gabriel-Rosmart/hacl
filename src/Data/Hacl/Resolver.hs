module Data.Hacl.Resolver where 

import Text.Megaparsec
import Data.Hacl.Parser
import Data.Hacl.Types
import qualified Data.Map as M
import qualified Data.Text as T

haclFromFile :: String -> IO Hacl
haclFromFile filename = load [] filename filename

-- Try to parse a given file if it is not in the banned list.
-- A banned list is kept in order to ensure no circular imports happen

load :: [String] -> String -> String -> IO Hacl
load banned caller filename = do
  if (filename `elem` banned) then error ("Forbidden recursive import, file " ++ caller ++ " tried to import " ++ filename) else do
    contents <- readFile filename
    case parse haclParser filename (T.pack contents) of
      Left e -> error (filename ++ " failed to parse with the following error:\n" ++ errorBundlePretty e)
      Right p -> resolveImports (banned ++ [filename]) filename p


-- Scan all pairs of an object and try to resolve imports if any

resolveImports :: [String] -> String -> Hacl -> IO Hacl
resolveImports banned caller (HaclObject o) = do 
  let intermediate = map (resolvePair banned caller) (M.toList o) :: [(T.Text, IO Hacl)]
  HaclObject <$> M.fromList <$> (sequenceA (map sequenceA intermediate))

-- Only objects are allowed to have imports, trying to resolve
-- any other type results in an error

resolveImports _ _ _ = error "Only objects can resolve imports"

resolvePair :: [String] -> String -> (T.Text, Hacl) -> (T.Text, IO Hacl)
resolvePair banned caller (t, HaclObject o) = (t, resolveImports banned caller (HaclObject o))
resolvePair banned caller (t, HaclImport i) = (t, load banned caller (T.unpack i))
resolvePair _ _ (t, e) = (t, return e)

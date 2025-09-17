module Resolver where 

import Text.Megaparsec
import Hacl
import HaclType
import qualified Data.Map as M
import qualified Data.Text as T

haclFromFile :: String -> IO Hacl
haclFromFile filename = load [] filename

-- Try to parse a given file if it is not in the banned list.
-- A banned list is kept in order to ensure no circular imports happen

load :: [String] -> String -> IO Hacl
load banned filename = do
  if (filename `elem` banned) then error ("Forbidden recursive import at file " ++ filename) else do
    contents <- readFile filename
    case parse haclParser filename (T.pack contents) of
      Left _ -> error "Could not load file"
      Right p -> resolveImports (banned ++ [filename]) p


-- Scan all pairs of an object and try to resolve imports if any

resolveImports :: [String] -> Hacl -> IO Hacl
resolveImports banned (HaclObject o) = do 
  let intermediate = map (resolvePair banned) (M.toList o) :: [(T.Text, IO Hacl)]
  HaclObject <$> M.fromList <$> (sequenceA (map sequenceA intermediate))

-- Only objects are allowed to have imports, trying to resolve
-- any other type results in an error

resolveImports _ _ = undefined

resolvePair :: [String] -> (T.Text, Hacl) -> (T.Text, IO Hacl)
resolvePair banned (t, HaclObject o) = (t, resolveImports banned (HaclObject o))
resolvePair banned (t, HaclImport i) = (t, load banned (T.unpack i))
resolvePair _ (t, e) = (t, return e)

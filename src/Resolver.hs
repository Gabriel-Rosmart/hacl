module Resolver where 

import Text.Megaparsec
import Hacl
import HaclType
import qualified Data.Map as M
import qualified Data.Text as T

haclFromFile :: String -> IO Hacl
haclFromFile filename = do 
  contents <- readFile filename
  case parse haclParser filename (T.pack contents) of
    Left _ -> error "Could not parse document"
    Right p -> return p


resolveImports :: Hacl -> IO Hacl
resolveImports (HaclObject o) = do 
  let intermediate = map resolvePair (M.toList o)
  HaclObject <$> M.fromList <$> (sequenceA (map sequenceA intermediate))
resolveImports _ = undefined

resolvePair :: (T.Text, Hacl) -> (T.Text, IO Hacl)
resolvePair (t, HaclObject o) = (t, resolveImports (HaclObject o))
resolvePair (t, HaclImport i) = (t, haclFromFile (T.unpack i))
resolvePair (t, e) = (t, return e)

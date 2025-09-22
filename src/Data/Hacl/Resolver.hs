module Data.Hacl.Resolver where 

import Text.Megaparsec
import Data.Hacl.Parser
import Data.Hacl.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Trans.Class(lift)
import Data.Hacl.Error
import System.Directory

type IOHacl = ExceptT HaclError IO Hacl
type IOString = ExceptT HaclError IO String
type FileList = [FilePath]

haclFromFile :: FilePath -> IO (Either HaclError Hacl) 
haclFromFile filename = runExceptT (load [] filename filename)

-- Try to parse a given file if it is not in the banned list.
-- A banned list is kept in order to ensure no circular imports happen

load :: FileList -> FilePath -> FilePath -> IOHacl
load banned ccaller filename = do
  if (filename `elem` banned) then ExceptT (return $ Left (RecursiveImport ccaller filename)) else do 
    (tryReadFile filename) >>= (\contents -> 
      case parse haclParser filename (T.pack contents) of
        Left e -> ExceptT (return $ Left (MegaparsecParseError e)) 
        Right p -> resolveImports (banned ++ [filename]) filename p
      )


-- Scan all pairs of an object and try to resolve imports if any

resolveImports :: FileList -> FilePath -> Hacl -> IOHacl
resolveImports banned ccaller (HaclObject o) = do 
  let intermediate = map (resolvePair banned ccaller) (M.toList o) :: [(T.Text, IOHacl)]
  HaclObject <$> M.fromList <$> (sequenceA (map sequenceA intermediate))

-- Only objects are allowed to have imports, trying to resolve
-- any other type results in an error

resolveImports _ _ _ = error "Only objects can resolve imports"

resolvePair :: FileList -> FilePath -> (T.Text, Hacl) -> (T.Text, IOHacl)
resolvePair banned ccaller (t, HaclObject o) = (t, resolveImports banned ccaller (HaclObject o))
resolvePair banned ccaller (t, HaclImport i) = (t, load banned ccaller (T.unpack i))
resolvePair _ _ (t, e) = (t, return e)


tryReadFile :: FilePath -> IOString
tryReadFile file = (lift $ doesFileExist file) >>= (\b -> if b == False then
    ExceptT (return $ Left (ResourceNotFound file)) else
    (lift $ readFile file) >>= (\c -> ExceptT (return $ Right c))
  )

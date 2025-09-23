module Data.Hacl.Resolver where 

import Text.Megaparsec
import Data.Hacl.Parser
import Data.Hacl.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class(lift)
import Data.Hacl.Error
import System.Directory

type FileList = [FilePath]

haclFromFile :: FilePath -> IO (Either HaclError Hacl) 
haclFromFile filename = runExceptT (load [] filename filename)

-- Try to parse a given file if it is not in the banned list.
-- A banned list is kept in order to ensure no circular imports happen

load :: FileList -> FilePath -> FilePath -> ExceptT HaclError IO Hacl
load banned ccaller filename = do
  if (filename `elem` banned) then ExceptT (return $ Left (RecursiveImport ccaller filename)) else do 
    (tryReadFile filename) >>= (\contents -> 
      case parse haclParser filename (T.pack contents) of
        Left e -> ExceptT (return $ Left (MegaparsecParseError e)) 
        Right p -> resolveImports (banned ++ [filename]) filename p
      )


-- Scan all pairs of an object and try to resolve imports if any.
-- Only objects are allowed to have imports, trying to resolve any other type results in an error

resolveImports :: FileList -> FilePath -> Hacl -> ExceptT HaclError IO Hacl
resolveImports banned ccaller (HaclObject object) = HaclObject <$> M.fromList <$>
  (sequenceA (map (resolvePair banned ccaller) (M.toList object)))

resolveImports _ _ _ = error "Data.Hacl.Error <resolveImports> function cannot be called to anything other than an object"

-- Try to resolve imports of a pair if any

resolvePair :: FileList -> FilePath -> (T.Text, Hacl) -> ExceptT HaclError IO (T.Text, Hacl)
resolvePair banned ccaller (t, HaclObject o) = sequenceA (t, resolveImports banned ccaller (HaclObject o))
resolvePair banned ccaller (t, HaclImport i) = sequenceA (t, load banned ccaller (T.unpack i))
resolvePair _ _ (t, e) = return (t, e)

-- Try to read a file failing if it does not exist

tryReadFile :: FilePath -> ExceptT HaclError IO String
tryReadFile file = (lift $ doesFileExist file) >>= (\b -> if b == False then
    ExceptT (return $ Left (ResourceNotFound file)) else
    (lift $ readFile file) >>= (\c -> ExceptT (return $ Right c))
  )

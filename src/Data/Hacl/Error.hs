module Data.Hacl.Error where 

import Text.Megaparsec.Error(ParseErrorBundle)
import Data.Void
import qualified Data.Text as T

data HaclError = RecursiveImport { caller :: FilePath, callee :: FilePath } 
  | MegaparsecParseError (ParseErrorBundle T.Text Void) 
  | ResourceNotFound { resource :: FilePath } deriving (Eq, Show)


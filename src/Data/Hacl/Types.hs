module Data.Hacl.Types where

import qualified Data.Text as T
import Data.Scientific
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

data Hacl = HaclText T.Text |
            HaclBool Bool |
            HaclNothing |
            HaclNumber Scientific |
            HaclArray (NE.NonEmpty Hacl) |
            HaclObject (M.Map T.Text Hacl) |
            HaclImport T.Text
            deriving (Eq, Show)

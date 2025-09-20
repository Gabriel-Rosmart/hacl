module Data.Hacl.Types where

import qualified Data.Text as T
import Data.Scientific
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

data Hacl = HaclText T.Text |
            HaclBool Bool |
            HaclNothing |
            HaclNumber HaclNumberType |
            HaclArray (NE.NonEmpty Hacl) |
            HaclObject (M.Map T.Text Hacl) |
            HaclImport T.Text
            deriving (Eq, Show)

data HaclNumberType = HaclInteger Int | HaclDouble Double deriving (Eq, Show)

castHaclNumber :: Scientific -> HaclNumberType
castHaclNumber n | (isFloating n) = HaclDouble (toRealFloat n)
                 | otherwise = HaclInteger (fromMaybe 0 (toBoundedInteger n))



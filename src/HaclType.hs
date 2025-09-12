module HaclType where

import Data.Text
import Data.Scientific
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

data Hacl = HaclText Text |
            HaclBool Bool |
            HaclNothing |
            HaclNumber HaclNumberType |
            HaclArray (NE.NonEmpty Hacl) |
            HaclObject (M.Map Text Hacl) |
            HaclImport Text
            deriving Show

data HaclNumberType = HaclInteger Int | HaclDouble Double deriving Show

castHaclNumber :: Scientific -> HaclNumberType
castHaclNumber n | (isFloating n) = HaclDouble (toRealFloat n)
                 | otherwise = HaclInteger (fromMaybe 0 (toBoundedInteger n))

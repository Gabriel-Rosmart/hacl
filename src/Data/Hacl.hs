module Data.Hacl where

import Data.Hacl.Types

class FromHacl a where
  decode :: Hacl -> a

class ToHacl a where 
  encode :: a -> Hacl

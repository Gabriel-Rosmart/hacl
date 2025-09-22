module Data.Hacl where

import Data.Hacl.Types
import Data.Hacl.Error
import Data.Hacl.Pretty

class FromHacl a where
  decode :: Hacl -> a

class ToHacl a where 
  encode :: a -> Hacl

dbg :: Either HaclError Hacl -> String
dbg (Left e) = prettyError e 
dbg (Right p) = pretty p

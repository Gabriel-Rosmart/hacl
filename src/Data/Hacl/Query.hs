module Data.Hacl.Query where 

import Data.Hacl.Types
import qualified Data.Map as M
import qualified Data.Text as T

key :: T.Text -> Hacl -> Maybe Hacl

key k (HaclObject o) = case M.lookup k o of
  Nothing -> if M.null objs then Nothing else (safeHead $ map (\p -> snd p) (M.toList found))
  Just v -> Just v
  where objs = M.filter isHaclObject o
        found = M.mapMaybe (key k) objs


key _ _ = error "key can only be used on objects"

keys :: T.Text -> Hacl -> Maybe [Hacl]
keys k (HaclObject o) = case M.lookup k o of
  Nothing -> if M.null objs then Nothing else Just wrapped
  Just v -> Just ([v] ++ wrapped)
  where objs = M.filter isHaclObject o
        found = M.mapMaybe (keys k) objs
        wrapped = concat $ map (\p -> snd p) (M.toList found)

keys _ _ = error "keys can only be used on objects"

isHaclObject :: Hacl -> Bool
isHaclObject (HaclObject _) = True
isHaclObject _ = False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

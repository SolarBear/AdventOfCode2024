module Utils
  (fromMaybeString)
where

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Prelude (negate)

fromMaybeString :: Maybe String -> Int
fromMaybeString Nothing = -1
fromMaybeString (Just x) = case fromString x of
  Nothing -> -1
  Just n -> n

module Day2
  (day2)
where

import Data.Array (toUnfoldable)
import Data.Int (decimal, fromString, toStringAs)
import Data.List (List(..), filter, length, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs, between)
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, map, negate, (#), ($), (&&), (-), (>))

splitFile :: String -> Array String
splitFile content = trim content # lines

lineToList :: String -> List Int
lineToList s = split patt s # map (\x -> fromString x # fromMaybe (-1)) # toUnfoldable
  where patt = Pattern " "

data Direction = Increasing | Decreasing

direction :: Int -> Int -> Direction
direction prev next = if prev > next then Decreasing else Increasing

isValidReport :: List Int -> Boolean
isValidReport l = go Nothing Nothing l where
  go :: Maybe Int -> Maybe Direction -> List Int -> Boolean
  go _ _ Nil = true
  go Nothing _ (x : xs) = go (Just x) Nothing xs
  go (Just prev) Nothing (x : xs) = if isValidRange (abs $ (prev - x)) then go (Just x) (Just $ direction prev x) xs else false
  go (Just prev) (Just dir) (x : xs) = isValidValue prev x dir && go (Just x) (Just dir) xs

isValidRange :: Int -> Boolean
isValidRange = between 1 3 

isValidValue :: Int -> Int -> Direction -> Boolean
isValidValue prev next Increasing = isValidRange (next - prev)
isValidValue prev next Decreasing = isValidRange (prev - next)

day2 :: Effect Unit
day2 = do
  inputFile <- readTextFile UTF8 "./data/input2.txt"
  let reports = splitFile inputFile # map lineToList
  let valids = map isValidReport reports # toUnfoldable
  let nbValid = filter (\x -> x) valids # length
  log $ toStringAs decimal nbValid

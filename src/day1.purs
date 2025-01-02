module Day1
  (day1)
where

import Data.Array (length, toUnfoldable, sort, unzip, zip, (!!))
import Data.Foldable (sum)
import Data.Int (decimal, fromString, toStringAs)
import Data.List (List)
import Data.Ord (abs)
import Data.Maybe
import Data.String.Pattern
import Data.String.Common (joinWith, split, trim)
import Data.String.Utils (lines)
import Data.Tuple
import Effect
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (bind, discard, map, negate, show, Unit, (#), ($), (-), (<>), (/=) )

type Point = Tuple Int Int

separator :: String
separator = "   "

fromMaybeString :: Maybe String -> Int
fromMaybeString Nothing = -1
fromMaybeString (Just x) = case fromString x of
  Nothing -> -1
  Just n -> n

toPoint :: Array String -> Point
toPoint arr = Tuple (fromMaybeString $ arr !! 0) (fromMaybeString $ arr !! 1)

splitFile :: String -> Array Point
splitFile content = trim content # lines # map (\x -> toPoint $ split patt x)
  where patt = Pattern separator

distance :: Point -> Int
distance p = abs $ (fst p) - (snd p)

day1 :: Effect Unit
day1 = do
  inputFile <- readTextFile UTF8 "./data/input1.txt"
  let numbers = splitFile inputFile
  let unzipped = unzip numbers
  let ordered = Tuple (fst unzipped # sort) (snd unzipped # sort)
  let pairs = zip (fst ordered) (snd ordered)
  let distances = map distance pairs
  let totalDistance = sum distances
  log $ toStringAs decimal totalDistance

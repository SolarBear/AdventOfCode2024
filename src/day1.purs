module Day1
  (day1)
where

import Data.Array (filter, length, sort, unzip, zip, (!!))
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.Ord (abs)
import Data.Maybe (Maybe(..))
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split, trim)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (bind, discard, map, negate, Unit, (#), ($), (-), (*), (==) )

import Utils (fromMaybeString)

type Point = Tuple Int Int

separator :: String
separator = "   "

toPoint :: Array String -> Point
toPoint arr = Tuple (fromMaybeString $ arr !! 0) (fromMaybeString $ arr !! 1)

splitFile :: String -> Array Point
splitFile content = trim content # lines # map (\x -> toPoint $ split patt x)
  where patt = Pattern separator

distance :: Point -> Int
distance p = abs $ (fst p) - (snd p)

similarity :: Array Int -> Int -> Int
similarity arr n = n * repeats
  where repeats = filter (_ == n) arr # length

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

  -- part 2
  let similarities = map (similarity $ snd ordered) (fst ordered)
  let totalSim = sum similarities
  log $ toStringAs decimal totalSim

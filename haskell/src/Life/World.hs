module Life.World where

import Data.List       (intercalate, find)
import Data.List.Split (chunksOf)
import Data.Maybe      (fromJust, isJust)
import Life.Cell

type World = [Cell]

nextWorld :: World -> World
nextWorld w = map (\c -> nextCell c $ livingNeighbors c w) w

showWorld :: World -> [Char]
showWorld w = intercalate "\n" rs
  where rs = map (map showCell) (rows w)

rows :: World -> [[Cell]]
rows w = chunksOf (width w) w

width :: World -> Int
width w = length $ filter (\(_, y, _) -> y == 0) w

livingNeighbors :: Cell -> World -> Int
livingNeighbors (x, y, _) w = length $ filter alive cs
  where cs = map (\(x', y') -> fromJust $ cellAt x' y' w)
                 (neighborCoords x y w)

neighborCoords :: Int -> Int -> World -> [(Int, Int)]
neighborCoords x y w = filter (inWorld w) $
  [(x-1, y-1), (x, y-1), (x+1, y-1),
   (x-1, y),             (x+1, y),
   (x-1, y+1), (x, y+1), (x+1, y+1)]
     where inWorld w (x, y) = isJust $ cellAt x y w

cellAt :: Int -> Int -> World -> Maybe Cell
cellAt x y = find (\(x', y', _) -> x == x' && y == y')

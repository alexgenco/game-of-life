module Life.Reader where

import Life.Cell
import Life.World

readWorld :: [Char] -> World
readWorld chs = readWorld' chs [] 0 0

readWorld' :: [Char] -> [Cell] -> Int -> Int -> World
readWorld' []       cs _ _ = cs
readWorld' (ch:chs) cs x y
  | ch == '\n' = readWorld' chs cs   0   (y+1)
  | otherwise  = readWorld' chs cs' (x+1) y
    where cs' = cs ++ [readCell x y ch]

readCell :: Int -> Int -> Char -> Cell
readCell x y 'o' = (x, y, Alive)
readCell x y  _  = (x, y, Dead)

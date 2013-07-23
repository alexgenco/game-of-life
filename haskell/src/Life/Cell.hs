module Life.Cell where

data State = Alive | Dead deriving (Show)
type Cell = (Int, Int, State)

nextCell :: Cell -> Int -> Cell
nextCell (x, y, s) n = (x, y, nextState s n)

nextState :: State -> Int -> State
nextState Alive 2 = Alive
nextState Alive 3 = Alive
nextState Alive _ = Dead
nextState Dead  3 = Alive
nextState _     _ = Dead

showCell :: Cell -> Char
showCell (_, _, Alive) = 'o'
showCell (_, _, Dead)  = '.'

alive :: Cell -> Bool
alive (_, _, Alive) = True
alive (_, _, Dead)  = False

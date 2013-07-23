module Main (main) where

import System.IO           (getContents)
import System.Posix.Unistd (usleep)
import Life.Reader         (readWorld)
import Life.World          (World, nextWorld, showWorld)

main :: IO ()
main = do
  getContents >>= loop

loop :: String -> IO ()
loop s = do
  tick
  nextWorld <- printAndNext (readWorld s)
  loop $ showWorld nextWorld

printAndNext :: World -> IO (World)
printAndNext w = do
  resetCursor
  clearScreen
  putStrLn $ showWorld w
  return $ nextWorld w

tick :: IO ()
tick = usleep 100000

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

resetCursor :: IO ()
resetCursor = putStr "\ESC[0;0H"

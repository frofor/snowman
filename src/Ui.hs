module Ui (
    drawFinished,
    drawTrapped,
    drawUi,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
  )
where

import Board (Board, Tile (..))
import Player (Player (..))
import System.Console.ANSI (clearScreen)

drawUi :: Board -> IO ()
drawUi board = do
  clearScreen
  putStrLn "'k' - Up"
  putStrLn "'h' - Left"
  putStrLn "'l' - Right"
  putStrLn "'q' - Quit"
  putStrLn ""
  drawBoard board

drawBoard :: Board -> IO ()
drawBoard = mapM_ (putStrLn . unwords . map showTile)

showTile :: Tile -> String
showTile Empty = "."
showTile (Head Red) = "\ESC[31m@\ESC[0m"
showTile (Head Blue) = "\ESC[34m^\ESC[0m"
showTile (Tail Red) = "\ESC[31m*\ESC[0m"
showTile (Tail Blue) = "\ESC[34m*\ESC[0m"
showTile Block = " "

drawFinished :: Player -> IO ()
drawFinished player = putStrLn $ showPlayer player ++ " reached to the top!"

drawTrapped :: Player -> IO ()
drawTrapped player = putStrLn $ showPlayer player ++ " trapped other player!"

showPlayer :: Player -> String
showPlayer Red = "\ESC[31mRED\ESC[0m"
showPlayer Blue = "\ESC[34mBLUE\ESC[0m"

warnInvalidInput :: Char -> IO ()
warnInvalidInput input = warn $ "Invalid input: '" ++ [input] ++ "'"

warnOutOfBounds :: IO ()
warnOutOfBounds = warn "Can't move out of bounds!"

warnOccupied :: IO ()
warnOccupied = warn "Tile is already occupied!"

warn :: String -> IO ()
warn text = do
  putStrLn ""
  putStrLn text
  putStrLn "Press any key to continue..."
  getChar >> pure ()

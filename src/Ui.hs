module Ui
  ( drawFinished,
    drawTrapped,
    drawUi,
    warnBoardParseError,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
  )
where

import Board (Board, Tile (TileBlock, TileEmpty, TileFinish, TileHead, TileTail))
import Control.Monad (void)
import Player (Player (Blue, Red))
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
drawBoard = mapM_ $ putStrLn . unwords . map showTile

showTile :: Tile -> String
showTile TileEmpty = "."
showTile (TileHead Red) = "\ESC[31m@\ESC[0m"
showTile (TileHead Blue) = "\ESC[34m^\ESC[0m"
showTile (TileTail Red) = "\ESC[31m*\ESC[0m"
showTile (TileTail Blue) = "\ESC[34m*\ESC[0m"
showTile TileFinish = "~"
showTile TileBlock = "x"

drawFinished :: Player -> IO ()
drawFinished = putStrLn . (++ " finished the game!") . showPlayer

drawTrapped :: Player -> IO ()
drawTrapped = putStrLn . (++ " trapped other player!") . showPlayer

showPlayer :: Player -> String
showPlayer Red = "\ESC[31mRED\ESC[0m"
showPlayer Blue = "\ESC[34mBLUE\ESC[0m"

warnBoardParseError :: IO ()
warnBoardParseError = warn "Can't parse board file!"

warnInvalidInput :: Char -> IO ()
warnInvalidInput = warn . ("Invalid input: '" ++) . show

warnOutOfBounds :: IO ()
warnOutOfBounds = warn "Can't move out of bounds!"

warnOccupied :: IO ()
warnOccupied = warn "Tile is already occupied!"

warn :: String -> IO ()
warn text = do
  putStrLn ""
  putStrLn text
  putStrLn "Press any key to continue..."
  void getChar

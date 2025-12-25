module Ui
  ( drawGameOver,
    drawUi,
    warnBoardParseError,
    warnFinished,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
    warnTrapped,
  )
where

import Board (Board, Tile (TileBlock, TileEmpty, TileHead, TileTail))
import Control.Monad (void)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Game (Game (Game, board, players))
import Player (Player (score), PlayerColor (PlayerBlue, PlayerRed))
import System.Console.ANSI (clearScreen)

drawUi :: Game -> IO ()
drawUi Game {board, players} = do
  clearScreen
  putStrLn "'k' - Up"
  putStrLn "'h' - Left"
  putStrLn "'l' - Right"
  putStrLn "'q' - Quit"
  putStrLn ""
  drawScore players
  putStrLn ""
  drawBoard board

drawScore :: Map PlayerColor Player -> IO ()
drawScore = putStrLn . intercalate " : " . map (show . score) . Map.elems

drawBoard :: Board -> IO ()
drawBoard = mapM_ $ putStrLn . unwords . map showTile

showTile :: Tile -> String
showTile TileEmpty = "."
showTile (TileHead PlayerRed) = "\ESC[31mo\ESC[0m"
showTile (TileHead PlayerBlue) = "\ESC[34mo\ESC[0m"
showTile TileTail = "o"
showTile TileBlock = "x"

drawGameOver :: PlayerColor -> IO ()
drawGameOver playerColor = do
  putStrLn ""
  putStrLn $ showPlayerColor playerColor ++ " won the game!"

showPlayerColor :: PlayerColor -> String
showPlayerColor PlayerRed = "\ESC[31mRED\ESC[0m"
showPlayerColor PlayerBlue = "\ESC[34mBLUE\ESC[0m"

warnBoardParseError :: IO ()
warnBoardParseError = warn "Can't parse board file!"

warnInvalidInput :: Char -> IO ()
warnInvalidInput = warn . ("Invalid input: " ++) . show

warnOutOfBounds :: IO ()
warnOutOfBounds = warn "Can't move out of bounds!"

warnOccupied :: IO ()
warnOccupied = warn "Tile is already occupied!"

warnFinished :: PlayerColor -> IO ()
warnFinished = warn . (++ " finished the game!") . showPlayerColor

warnTrapped :: PlayerColor -> IO ()
warnTrapped = warn . (++ " trapped other player!") . showPlayerColor

warn :: String -> IO ()
warn text = do
  putStrLn ""
  putStrLn text
  putStrLn "Press any key to continue..."
  void getChar

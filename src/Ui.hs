module Ui
  ( drawBoardNotFound,
    drawBoardParseError,
    drawGameOver,
    drawUi,
    warnFinished,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
    warnTrapped,
  )
where

import Board
  ( Board,
    BoardParseError (HeadNotFound, InvalidTile, MultipleHeads),
    Tile (TileBlock, TileEmpty, TileHead, TileTail),
  )
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
  putStrLn "'k', '↑' - Up"
  putStrLn "'h', '←' - Left"
  putStrLn "'l', '→' - Right"
  putStrLn "'q' - Quit"
  putStrLn ""
  drawScore players
  putStrLn ""
  drawBoard board

drawScore :: Map PlayerColor Player -> IO ()
drawScore = putStrLn . intercalate " : " . map (show . score) . Map.elems

drawBoard :: Board -> IO ()
drawBoard = mapM_ $ putStrLn . unwords . map showTile

drawBoardNotFound :: IO ()
drawBoardNotFound =
  putStrLn "No \x1b[34m*.board\x1b[0m file found in \x1b[34mresources/boards/\x1b[0m"

drawBoardParseError :: String -> BoardParseError -> IO ()
drawBoardParseError name e =
  putStrLn $
    "Failed to parse \x1b[34m" ++ name ++ "\x1b[0m: " ++ case e of
      InvalidTile t -> "Invalid tile: " ++ show t
      HeadNotFound -> "Head doesn't exist"
      MultipleHeads -> "Board contains multiple heads"

showTile :: Tile -> String
showTile TileEmpty = "."
showTile (TileHead PlayerRed) = "\x1b[31mo\x1b[0m"
showTile (TileHead PlayerBlue) = "\x1b[34mo\x1b[0m"
showTile TileTail = "o"
showTile TileBlock = "x"

drawGameOver :: PlayerColor -> IO ()
drawGameOver playerColor = do
  putStrLn ""
  putStrLn $ showPlayerColor playerColor ++ " won the game!"

showPlayerColor :: PlayerColor -> String
showPlayerColor PlayerRed = "\x1b[31mRED\x1b[0m"
showPlayerColor PlayerBlue = "\x1b[34mBLUE\x1b[0m"

warnInvalidInput :: String -> IO ()
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

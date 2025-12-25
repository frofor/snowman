import Board (Board)
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Game
  ( Game (Game, board, playerColor, players),
    GameState (GameFinished, GameOngoing, GameTrapped),
    MoveDirection (MoveLeft, MoveRight, MoveUp),
    MoveError (MoveOccupied, MoveOutOfBounds),
    getGameState,
    initGame,
    move,
  )
import Player (Player (color, score), PlayerColor (PlayerRed), prevPlayerColor)
import Resource (loadRandomBoard)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import Ui
  ( drawGameOver,
    drawUi,
    warnBoardParseError,
    warnFinished,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
    warnTrapped,
  )

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  board <- tryLoadBoard
  gameLoop $ initGame board

gameLoop :: Game -> IO ()
gameLoop game@Game {board} = do
  when (getGameState board /= GameOngoing) $ onWin game

  drawUi game

  input <- getChar
  game' <- case input of
    'k' -> tryMove MoveUp game
    'h' -> tryMove MoveLeft game
    'l' -> tryMove MoveRight game
    'q' -> exitSuccess
    _ -> warnInvalidInput input >> pure Nothing
  maybe (gameLoop game) gameLoop game'

onWin :: Game -> IO ()
onWin game@Game {board, players, playerColor} = do
  prevPlayer <- case Map.lookup (prevPlayerColor playerColor) players of
    Just p -> pure p
    Nothing -> error "Game should contain multiple players"

  board' <- tryLoadBoard

  let prevPlayer' = prevPlayer {score = score prevPlayer + 1}
      players' = Map.insert (color prevPlayer) prevPlayer' players
      game' = game {players = players', playerColor = PlayerRed}

  drawUi game'

  when (score prevPlayer' == 5) $ do
    drawGameOver $ color prevPlayer
    exitSuccess

  case getGameState board of
    GameOngoing -> error "Game should not be ongoing"
    GameTrapped -> warnTrapped $ color prevPlayer
    GameFinished -> warnFinished $ color prevPlayer

  gameLoop game' {board = board'}

tryMove :: MoveDirection -> Game -> IO (Maybe Game)
tryMove d g = case move d g of
  Right g' -> pure $ Just g'
  Left MoveOutOfBounds -> warnOutOfBounds >> pure Nothing
  Left MoveOccupied -> warnOccupied >> pure Nothing

tryLoadBoard :: IO Board
tryLoadBoard = do
  board <- loadRandomBoard
  maybe (warnBoardParseError >> exitFailure) pure board

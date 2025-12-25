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
import Player (Player (color, score), PlayerColor (PlayerRed), nextPlayerColor, prevPlayerColor)
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

  board <- tryLoadBoard PlayerRed
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
  winner <- case Map.lookup (prevPlayerColor playerColor) players of
    Just p -> pure p
    Nothing -> error "Game should contain multiple players"

  let winner' = winner {score = score winner + 1}
      winnerColor = color winner
      players' = Map.insert winnerColor winner' players
      starterColor = nextPlayerColor winnerColor
      game' = game {players = players', playerColor = starterColor}

  drawUi game'

  when (score winner' == 5) $ do
    drawGameOver winnerColor
    exitSuccess

  case getGameState board of
    GameOngoing -> error "Game should not be ongoing"
    GameTrapped -> warnTrapped winnerColor
    GameFinished -> warnFinished winnerColor

  board' <- tryLoadBoard starterColor
  gameLoop game' {board = board'}

tryMove :: MoveDirection -> Game -> IO (Maybe Game)
tryMove d g = case move d g of
  Right g' -> pure $ Just g'
  Left MoveOutOfBounds -> warnOutOfBounds >> pure Nothing
  Left MoveOccupied -> warnOccupied >> pure Nothing

tryLoadBoard :: PlayerColor -> IO Board
tryLoadBoard playerColor = do
  board <- loadRandomBoard playerColor
  maybe (warnBoardParseError >> exitFailure) pure board

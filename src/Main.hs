import Game
  ( Game (Game, board, player),
    MoveDirection (MoveLeft, MoveRight, MoveUp),
    MoveError (MoveFinished, MoveOccupied, MoveOutOfBounds, MoveTrapped),
    move,
  )
import Player (Player (Red))
import Resource (loadRandomBoard)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import Ui
  ( drawFinished,
    drawTrapped,
    drawUi,
    warnBoardParseError,
    warnInvalidInput,
    warnOccupied,
    warnOutOfBounds,
  )

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  board <- loadRandomBoard
  board' <- maybe (warnBoardParseError >> exitFailure) pure board
  gameLoop Game {board = board', player = Red}

gameLoop :: Game -> IO ()
gameLoop game@Game {board} = do
  drawUi board

  input <- getChar
  game' <- case input of
    'k' -> tryMove MoveUp game
    'h' -> tryMove MoveLeft game
    'l' -> tryMove MoveRight game
    'q' -> exitSuccess
    _ -> warnInvalidInput input >> pure Nothing
  maybe (gameLoop game) gameLoop game'

tryMove :: MoveDirection -> Game -> IO (Maybe Game)
tryMove d g@Game {player} = case move d g of
  Right g' -> pure $ Just g'
  Left MoveOutOfBounds -> warnOutOfBounds >> pure Nothing
  Left MoveOccupied -> warnOccupied >> pure Nothing
  Left MoveFinished -> drawFinished player >> exitSuccess
  Left MoveTrapped -> drawTrapped player >> exitSuccess

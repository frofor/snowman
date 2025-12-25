import Game
  ( Game (Game, board, player),
    GameState (GameFinished, GameOngoing, GameTrapped),
    MoveDirection (MoveLeft, MoveRight, MoveUp),
    MoveError (MoveOccupied, MoveOutOfBounds),
    getGameState,
    move,
  )
import Player (Player (Red), prevPlayer)
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
gameLoop game@Game {board, player} = do
  drawUi board

  case getGameState board of
    GameOngoing -> pure ()
    GameTrapped -> drawTrapped (prevPlayer player) >> exitSuccess
    GameFinished -> drawFinished (prevPlayer player) >> exitSuccess

  input <- getChar
  game' <- case input of
    'k' -> tryMove MoveUp game
    'h' -> tryMove MoveLeft game
    'l' -> tryMove MoveRight game
    'q' -> exitSuccess
    _ -> warnInvalidInput input >> pure Nothing
  maybe (gameLoop game) gameLoop game'

tryMove :: MoveDirection -> Game -> IO (Maybe Game)
tryMove d g = case move d g of
  Right g' -> pure $ Just g'
  Left MoveOutOfBounds -> warnOutOfBounds >> pure Nothing
  Left MoveOccupied -> warnOccupied >> pure Nothing

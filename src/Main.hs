import Board (Board, initBoard)
import Game (GameState (..), MoveDirection (..), MoveError (..), getGameState, move)
import Player (Player (..), nextPlayer)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import Ui (drawFinished, drawTrapped, drawUi, warnInvalidInput, warnOccupied, warnOutOfBounds)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  repl (nextPlayer Red) initBoard

repl :: Player -> Board -> IO ()
repl player board = do
  drawUi board

  case getGameState board of
    Ongoing -> pure ()
    Finished -> drawFinished player >> exitSuccess
    Trapped -> drawTrapped player >> exitSuccess

  input <- getChar
  board' <- case input of
    'k' -> tryMove MoveUp player board
    'h' -> tryMove MoveLeft player board
    'l' -> tryMove MoveRight player board
    'q' -> exitSuccess
    _ -> warnInvalidInput input >> pure Nothing

  case board' of
    Just b -> repl (nextPlayer player) b
    Nothing -> repl player board

tryMove :: MoveDirection -> Player -> Board -> IO (Maybe Board)
tryMove d p b = case move d p b of
  Right b' -> pure $ Just b'
  Left OutOfBounds -> warnOutOfBounds >> pure Nothing
  Left Occupied -> warnOccupied >> pure Nothing

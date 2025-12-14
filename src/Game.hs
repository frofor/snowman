module Game (GameState (..), MoveDirection (..), MoveError (..), getGameState, move) where

import Board (Board, Tile (..), Vec, findHead)
import Player (Player, prevPlayer)

data GameState = Ongoing | Finished | Trapped deriving (Eq)

data MoveDirection = MoveUp | MoveLeft | MoveRight

data MoveError = OutOfBounds | Occupied

move :: MoveDirection -> Player -> Board -> Either MoveError Board
move MoveUp = moveBy (0, -1)
move MoveLeft = moveBy (-1, 0)
move MoveRight = moveBy (1, 0)

moveBy :: Vec -> Player -> Board -> Either MoveError Board
moveBy (x, y) player board
  | hx < 0 || hx == length headRow = Left OutOfBounds
  | headRow !! hx /= Empty = Left Occupied
  | otherwise = Right $ moveHead tailPos headPos player board
  where
    tailPos@(tx, ty) = findHead board
    headPos@(hx, hy) = (tx + x, ty + y)
    headRow = board !! hy

moveHead :: Vec -> Vec -> Player -> Board -> Board
moveHead (x, y) (x', y') player board =
  let tailRow = board !! y
      tailRow' = take x tailRow ++ [Tail $ prevPlayer player] ++ drop (x + 1) tailRow
      board' = take y board ++ [tailRow'] ++ drop (y + 1) board
      headRow = board' !! y'
      headRow' = take x' headRow ++ [Head player] ++ drop (x' + 1) headRow
   in take y' board' ++ [headRow'] ++ drop (y' + 1) board'

getGameState :: Board -> GameState
getGameState board
  | y == 0 = Finished
  | trapped = Trapped
  | otherwise = Ongoing
  where
    (x, y) = findHead board
    row = board !! y
    trapped =
      board !! (y - 1) !! x /= Empty
        && (x == 0 || row !! (x - 1) /= Empty)
        && (x == length row - 1 || row !! (x + 1) /= Empty)

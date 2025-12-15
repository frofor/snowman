module Game
  ( Game (..),
    MoveDirection (..),
    MoveError (..),
    move,
  )
where

import Board (Board, Tile (TileFinish, TileHead, TileTail), findHead, isOccupied)
import Player (Player, nextPlayer)
import Types (Vec)

data Game = Game {board :: Board, player :: Player}

data MoveDirection = MoveUp | MoveLeft | MoveRight

data MoveError = MoveOutOfBounds | MoveOccupied | MoveFinished | MoveTrapped

move :: MoveDirection -> Game -> Either MoveError Game
move MoveUp = moveBy (0, -1)
move MoveLeft = moveBy (-1, 0)
move MoveRight = moveBy (1, 0)

moveBy :: Vec -> Game -> Either MoveError Game
moveBy (x, y) game@Game {board}
  | hy < 0 || hx < 0 || hx == length headRow = Left MoveOutOfBounds
  | isOccupied headCell = Left MoveOccupied
  | headCell == TileFinish = Left MoveFinished
  | (hy == 0 || isOccupied (board !! (hy - 1) !! hx))
      && (hx == 0 || isOccupied (headRow !! (hx - 1)))
      && (hx == length headRow - 1 || isOccupied (headRow !! (hx + 1))) =
      Left MoveTrapped
  | otherwise = Right $ moveTo headPos game
  where
    (tx, ty) = findHead board
    headPos@(hx, hy) = (tx + x, ty + y)
    headRow = board !! hy
    headCell = headRow !! hx

moveTo :: Vec -> Game -> Game
moveTo (x, y) game@Game {board, player} =
  let player' = nextPlayer player
      (tx, ty) = findHead board
      tailRow = board !! ty
      tailRow' = take tx tailRow ++ [TileTail player] ++ drop (tx + 1) tailRow
      board' = take ty board ++ [tailRow'] ++ drop (ty + 1) board
      headRow = board' !! y
      headRow' = take x headRow ++ [TileHead player'] ++ drop (x + 1) headRow
   in game {board = take y board' ++ [headRow'] ++ drop (y + 1) board', player = player'}

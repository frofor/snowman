module Game
  ( Game (..),
    GameState (..),
    MoveDirection (..),
    MoveError (..),
    getGameState,
    move,
  )
where

import Board (Board, Tile (TileEmpty, TileHead, TileTail), findHead, isOccupied)
import Player (Player, nextPlayer)
import Types (Vec)

data Game = Game {board :: Board, player :: Player}

data GameState = GameOngoing | GameFinished | GameTrapped

data MoveDirection = MoveUp | MoveLeft | MoveRight

data MoveError = MoveOutOfBounds | MoveOccupied

getGameState :: Board -> GameState
getGameState board
  | y == 0 = GameFinished
  | trapped = GameTrapped
  | otherwise = GameOngoing
  where
    (x, y) = findHead board
    row = board !! y
    trapped =
      board !! (y - 1) !! x /= TileEmpty
        && (x == 0 || row !! (x - 1) /= TileEmpty)
        && (x == length row - 1 || row !! (x + 1) /= TileEmpty)

move :: MoveDirection -> Game -> Either MoveError Game
move MoveUp = moveBy (0, -1)
move MoveLeft = moveBy (-1, 0)
move MoveRight = moveBy (1, 0)

moveBy :: Vec -> Game -> Either MoveError Game
moveBy (x, y) game@Game {board}
  | hy < 0 || hx < 0 || hx == length headRow = Left MoveOutOfBounds
  | isOccupied $ headRow !! hx = Left MoveOccupied
  | otherwise = Right $ moveTo headPos game
  where
    (tx, ty) = findHead board
    headPos@(hx, hy) = (tx + x, ty + y)
    headRow = board !! hy

moveTo :: Vec -> Game -> Game
moveTo (x, y) game@Game {board, player} =
  let player' = nextPlayer player
      (tx, ty) = findHead board
      tailRow = board !! ty
      tailRow' = take tx tailRow ++ [TileTail] ++ drop (tx + 1) tailRow
      board' = take ty board ++ [tailRow'] ++ drop (ty + 1) board
      headRow = board' !! y
      headRow' = take x headRow ++ [TileHead player'] ++ drop (x + 1) headRow
   in game {board = take y board' ++ [headRow'] ++ drop (y + 1) board', player = player'}

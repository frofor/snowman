module Board (Board, Tile (..), Vec, findHead, initBoard) where

import Player (Player (Red))

type Board = [[Tile]]

data Tile = Empty | Head Player | Tail Player | Block deriving (Eq)

type Vec = (Int, Int)

initBoard :: Board
initBoard =
  [ [Block, Empty, Empty, Block],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Head Red, Empty, Empty]
  ]

findHead :: Board -> Vec
findHead board =
  case [(x, y) | (y, row) <- zip [0 ..] board, (x, tile) <- zip [0 ..] row, isHead tile] of
    (h : _) -> h
    [] -> error "Board should contain head"

isHead :: Tile -> Bool
isHead (Head _) = True
isHead _ = False

module Board (Board, BoardParseError (..), Tile (..), findHead, isOccupied, parseBoard) where

import Data.Text (Text)
import Data.Text qualified as T
import Player
import Types (Vec)

type Board = [[Tile]]

data Tile = TileEmpty | TileHead PlayerColor | TileTail | TileBlock deriving (Eq)

data BoardParseError = InvalidTile String | HeadNotFound | MultipleHeads

parseBoard :: Text -> PlayerColor -> Either BoardParseError Board
parseBoard contents playerColor =
  let rows = filter (\l -> not (T.null l) && T.head l /= '#') . T.lines $ contents
   in mapM (mapM ((`parseTile` playerColor) . T.unpack) . T.words) rows >>= ensureSingleHead

parseTile :: String -> PlayerColor -> Either BoardParseError Tile
parseTile "." _ = Right TileEmpty
parseTile "x" _ = Right TileBlock
parseTile "o" c = Right $ TileHead c
parseTile s _ = Left $ InvalidTile s

ensureSingleHead :: Board -> Either BoardParseError Board
ensureSingleHead board = case concatMap (filter isHead) board of
  [] -> Left HeadNotFound
  [_] -> Right board
  _ -> Left MultipleHeads

findHead :: Board -> Vec
findHead board = case [(x, y) | (y, row) <- zip [0 ..] board, (x, t) <- zip [0 ..] row, isHead t] of
  [p] -> p
  _ -> error "Board should contain head"

isHead :: Tile -> Bool
isHead (TileHead _) = True
isHead _ = False

isOccupied :: Tile -> Bool
isOccupied TileEmpty = False
isOccupied _ = True

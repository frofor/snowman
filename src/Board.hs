module Board (Board, Tile (..), findHead, isOccupied, parseBoard) where

import Data.Text (Text)
import Data.Text qualified as T
import Player
import Types (Vec)

type Board = [[Tile]]

data Tile = TileEmpty | TileHead PlayerColor | TileTail | TileBlock deriving (Eq)

parseBoard :: Text -> PlayerColor -> Maybe Board
parseBoard contents playerColor =
  let rows = filter (\l -> not (T.null l) && T.head l /= '#') . T.lines $ contents
   in mapM (mapM ((`parseTile` playerColor) . T.unpack) . T.words) rows

parseTile :: String -> PlayerColor -> Maybe Tile
parseTile "." _ = Just TileEmpty
parseTile "x" _ = Just TileBlock
parseTile "o" c = Just $ TileHead c
parseTile _ _ = Nothing

findHead :: Board -> Vec
findHead board =
  case [(x, y) | (y, row) <- zip [0 ..] board, (x, cell) <- zip [0 ..] row, isHead cell] of
    (p : _) -> p
    _ -> error "Board should contain head"

isHead :: Tile -> Bool
isHead (TileHead _) = True
isHead _ = False

isOccupied :: Tile -> Bool
isOccupied TileEmpty = False
isOccupied _ = True

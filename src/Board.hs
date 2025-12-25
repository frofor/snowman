module Board (Board, Tile (..), findHead, isOccupied, parseBoard) where

import Data.Text (Text)
import Data.Text qualified as T
import Player (PlayerColor (PlayerRed))
import Types (Vec)

type Board = [[Tile]]

data Tile = TileEmpty | TileHead PlayerColor | TileTail | TileBlock deriving (Eq)

parseBoard :: Text -> Maybe Board
parseBoard contents =
  let rows = filter (\l -> not (T.null l) && T.head l /= '#') . T.lines $ contents
   in mapM (mapM (parseTile . T.unpack) . T.words) rows

parseTile :: String -> Maybe Tile
parseTile "." = Just TileEmpty
parseTile "x" = Just TileBlock
parseTile "o" = Just $ TileHead PlayerRed
parseTile _ = Nothing

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

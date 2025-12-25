module Resource (loadRandomBoard) where

import Board (Board, parseBoard)
import Data.List (isSuffixOf)
import Data.Text qualified as T
import Player (PlayerColor)
import System.Directory (getDirectoryContents)
import System.Random (randomRIO)

loadRandomBoard :: PlayerColor -> IO (Maybe Board)
loadRandomBoard playerColor = do
  names <- filter (".board" `isSuffixOf`) <$> getDirectoryContents "resources/boards"
  index <- randomRIO (0, length names - 1)
  loadBoard (names !! index) playerColor

loadBoard :: String -> PlayerColor -> IO (Maybe Board)
loadBoard name playerColor = do
  contents <- readFile $ "resources/boards/" ++ name
  pure $ parseBoard (T.pack contents) playerColor

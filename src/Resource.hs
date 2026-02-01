module Resource (RandomBoardLoadError (..), loadRandomBoard) where

import Board (Board, BoardParseError, parseBoard)
import Data.List (isSuffixOf)
import Data.Text qualified as T
import Player (PlayerColor)
import System.Directory (getDirectoryContents)
import System.Random (randomRIO)

data RandomBoardLoadError = RandomBoardLoadError {name :: String, origin :: BoardParseError}

loadRandomBoard :: PlayerColor -> IO (Maybe (Either RandomBoardLoadError Board))
loadRandomBoard playerColor = do
  names <- filter (".board" `isSuffixOf`) <$> getDirectoryContents "resources/boards"
  if null names
    then pure Nothing
    else do
      index <- randomRIO (0, length names - 1)
      let name = names !! index
      board <- loadBoard name playerColor
      pure $ Just $ either (\e -> Left $ RandomBoardLoadError {name, origin = e}) Right board

loadBoard :: String -> PlayerColor -> IO (Either BoardParseError Board)
loadBoard name playerColor = do
  contents <- readFile $ "resources/boards/" ++ name
  pure $ parseBoard (T.pack contents) playerColor

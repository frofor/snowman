module Resource (loadRandomBoard) where

import Board (Board, parseBoard)
import Data.List (isSuffixOf)
import Data.Text qualified as T
import System.Directory (getDirectoryContents)
import System.Random (randomRIO)

loadRandomBoard :: IO (Maybe Board)
loadRandomBoard = do
  names <- filter (".board" `isSuffixOf`) <$> getDirectoryContents "resources/boards"
  index <- randomRIO (0, length names - 1)
  loadBoard $ names !! index

loadBoard :: String -> IO (Maybe Board)
loadBoard name = parseBoard . T.pack <$> readFile ("resources/boards/" ++ name)

module Player (Player (..), nextPlayer) where

data Player = Red | Blue deriving (Eq)

nextPlayer :: Player -> Player
nextPlayer Red = Blue
nextPlayer Blue = Red

module Player (Player (..), nextPlayer, prevPlayer) where

data Player = Red | Blue deriving (Eq)

prevPlayer :: Player -> Player
prevPlayer Red = Blue
prevPlayer Blue = Red

nextPlayer :: Player -> Player
nextPlayer Red = Blue
nextPlayer Blue = Red

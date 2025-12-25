module Player (Player (..), PlayerColor (..), initPlayer, nextPlayerColor, prevPlayerColor) where

data Player = Player {color :: PlayerColor, score :: Int}

data PlayerColor = PlayerRed | PlayerBlue deriving (Eq, Ord)

initPlayer :: PlayerColor -> Player
initPlayer color = Player {color, score = 0}

prevPlayerColor :: PlayerColor -> PlayerColor
prevPlayerColor PlayerRed = PlayerBlue
prevPlayerColor PlayerBlue = PlayerRed

nextPlayerColor :: PlayerColor -> PlayerColor
nextPlayerColor PlayerRed = PlayerBlue
nextPlayerColor PlayerBlue = PlayerRed

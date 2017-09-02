import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe

data Face = Front | Right | Back | Left | Up | Down deriving (Ord, Eq, Enum, Show)
data Color = Red | Green | Orange | Blue | Yellow | White deriving (Ord, Eq, Enum, Show)

type Sticker = (Face, Color)

type Piece = [Sticker]

type Cube = [Piece]

hasColor :: Color -> Piece -> Bool
hasColor thisColor piece = thisColor `elem` colors piece

colors :: Piece -> [Color]
colors piece = map color piece

color :: Sticker -> Color
color sticker = snd sticker
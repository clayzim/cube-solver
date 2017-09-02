import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe

data Face = Front | Right | Back | Left | Up | Down deriving (Ord, Eq, Enum, Show)
data Color = Red | Green | Orange | Blue | Yellow | White deriving (Ord, Eq, Enum, Show)

type Sticker = (Face, Color)

type Piece = [Sticker]

type Cube = [Piece]
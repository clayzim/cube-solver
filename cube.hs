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

hasFace :: Face -> Piece -> Bool
hasFace thisFace piece = thisFace `elem` faces piece

faces :: Piece -> [Face]
faces piece = map face piece

face :: Sticker -> Face
face sticker = fst sticker

isSolved :: Cube -> Bool
isSolved cube = all inFinalPosition cube

inFinalPosition :: Piece -> Bool
inFinalPosition piece = all onCorrectFace piece

onCorrectFace :: Sticker -> Bool
onCorrectFace sticker = face sticker == appropriateFace sticker

appropriateFace :: Sticker -> Face
appropriateFace sticker
    | thisColor == Red = Front
    | thisColor == Green = Right
    | thisColor == Orange = Back
    | thisColor == Blue = Left
    | thisColor == Yellow = Up
    | thisColor == White = Down
    where thisColor = color sticker
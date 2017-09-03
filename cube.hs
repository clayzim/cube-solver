import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe

data Face = Front | Right | Back | Left | Up | Down deriving (Ord, Eq, Enum, Show)
data Color = Red | Green | Orange | Blue | Yellow | White deriving (Ord, Eq, Enum, Show)

type Sticker = (Face, Color)

type Piece = [Sticker]

type Cube = [Piece]

--Specific cube state to play with, since input is not yet implemented
exampleCube = [[(Up, Orange), (Front, Green), (Left, Yellow)],[(Front, Yellow), (Up, Orange)],[(Front, Green), (Up, Red), (Right, Yellow)],[(Front, Orange), (Left, Blue)],[(Front, Yellow), (Right, Green)],[(Front, Blue), (Left, Red), (Down, Yellow)],[(Front, Red), (Down, Green)],[(Front, Blue), (Right, Orange), (Down, Yellow)],[(Right, Blue), (Up, Red)],[(Right, White), (Down, Red)],[(Right, Green), (Back, White), (Up, Orange)],[(Right, Blue), (Back, White)],[(Right, Blue), (Back, White), (Down, Orange)],[(Back, Orange), (Up, Green)],[(Back, Blue), (Down, Yellow)],[(Back, White), (Left, Green), (Up, Red)],[(Back, Orange), (Left, White)],[(Back, White), (Left, Blue), (Down, Red)],[(Left, Yellow), (Up, Red)],[(Left, Green), (Down, White)]]

isEdge :: Piece -> Bool
isEdge piece = length piece == 2

isCorner :: Piece -> Bool
isCorner piece = length piece == 3

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

--Hard-coded. Maybe not the best-possible solution
appropriateFace :: Sticker -> Face
appropriateFace sticker
    | thisColor == Red = Front
    | thisColor == Green = Right
    | thisColor == Orange = Back
    | thisColor == Blue = Left
    | thisColor == Yellow = Up
    | thisColor == White = Down
    where thisColor = color sticker
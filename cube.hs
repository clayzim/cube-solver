import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe

data Face = Front | Right | Back | Left | Up | Down deriving (Ord, Eq, Enum, Show)
data Color = Red | Green | Orange | Blue | Yellow | White deriving (Ord, Eq, Enum, Show)

data Rotation = Clockwise | Counterclockwise deriving (Ord, Eq, Enum, Show)

type Sticker = (Face, Color)

type Piece = [Sticker]

type Cube = [Piece]

--Specific cube state to play with, since input is not yet implemented
exampleCube = [[(Up, Orange), (Front, Green), (Left, Yellow)],[(Front, Yellow), (Up, Orange)],[(Front, Green), (Up, Red), (Right, Yellow)],[(Front, Orange), (Left, Blue)],[(Front, Yellow), (Right, Green)],[(Front, Blue), (Left, Red), (Down, Yellow)],[(Front, Red), (Down, Green)],[(Front, Blue), (Right, Orange), (Down, Yellow)],[(Right, Blue), (Up, Red)],[(Right, White), (Down, Red)],[(Right, Green), (Back, White), (Up, Orange)],[(Right, Blue), (Back, White)],[(Right, Blue), (Back, White), (Down, Orange)],[(Back, Orange), (Up, Green)],[(Back, Blue), (Down, Yellow)],[(Back, White), (Left, Green), (Up, Red)],[(Back, Orange), (Left, White)],[(Back, White), (Left, Blue), (Down, Red)],[(Left, Yellow), (Up, Red)],[(Left, Green), (Down, White)]]

isEdge :: Piece -> Bool
isEdge piece = length piece == 2

isCorner :: Piece -> Bool
isCorner piece = length piece == 3

--There's only ever gonna be one sticker with the same color. A piece can't have duplicate colors
faceWithColor :: Color -> Piece -> Maybe Face
faceWithColor thisColor piece 
    | null stickersWithColor = Nothing
    | otherwise = Just $ face $ head stickersWithColor
    where stickersWithColor = filter (\sticker -> color sticker == thisColor) piece

--Rather than Maybe's, I might want to just allow the crash when called with a nonexistent face
colorOnFace :: Face -> Piece -> Maybe Color
colorOnFace thisFace piece
    | null stickersOnFace = Nothing
    | otherwise = Just $ color $ head stickersOnFace
    where stickersOnFace = filter (\sticker -> face sticker == thisFace) piece

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
onCorrectFace sticker = face sticker == correctFace sticker

--Hard-coded. Maybe not the best-possible solution
correctFace :: Sticker -> Face
correctFace sticker
    | thisColor == Red = Front
    | thisColor == Green = Right
    | thisColor == Orange = Back
    | thisColor == Blue = Left
    | thisColor == Yellow = Up
    | thisColor == White = Down
    where thisColor = color sticker

--Only expose this rotate function to any users of this module
rotate :: Face -> Rotation -> Cube -> Cube
rotate thisFace rotation cube = rotated ++ unchanged
    where facePieces = filter (hasFace thisFace) cube
          unchanged = filter (`notElem` facePieces) cube
          rotated = map (rotatePiece thisFace rotation) facePieces

rotatePiece :: Face -> Rotation -> (Piece -> Piece)
rotatePiece face rotation = map (nextSticker sequence)
    where baseSequence = clockwiseSequence face
          sequence = if (rotation == Clockwise)
                         then baseSequence
                         else reverse baseSequence

clockwiseSequence :: Face -> [Face]
clockwiseSequence face
    | face == Front = zClockwise
    | face == Right = xClockwise
    | face == Back = reverse zClockwise
    | face == Left = reverse xClockwise
    | face == Up = yClockwise
    | face == Down = reverse yClockwise
    where xClockwise = [Front,Up,Back,Down]
          yClockwise = [Front,Left,Back,Right]
          zClockwise = [Up,Right,Down,Left]

nextSticker :: [Face] -> Sticker -> Sticker
nextSticker sequence (face, color) = (nextInCycle sequence face, color)

nextInCycle :: (Eq a) => [a] -> a -> a
nextInCycle sequence currentValue
    | isJust currentIndex = progression !! (fromJust currentIndex + 1)
    | otherwise = currentValue
    where currentIndex = currentValue `elemIndex` sequence
          progression = cycle sequence






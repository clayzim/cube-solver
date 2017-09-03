import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe

data Face = Front | Right | Back | Left | Up | Down deriving (Ord, Eq, Enum, Show)
data Color = Red | Green | Orange | Blue | Yellow | White deriving (Ord, Eq, Enum, Show)

data Axis = X | Y | Z deriving (Ord, Eq, Enum, Show)
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

rotateCube :: Face -> Cube -> Cube
rotateCube face cube = rotated ++ unchanged
                       where facePieces = filter (hasFace face) cube
                             rotated = map (rotatePieceByFace face) facePieces
                             unchanged = filter (`notElem` facePieces) cube

--Potentially change order of axis & rotation arguments
rotatePiece :: Axis -> Rotation -> (Piece -> Piece)
rotatePiece axis rotation
    | rotation == Clockwise = map (nextSticker sequence)
    | otherwise = map (nextSticker $ reverse sequence)
    where sequence = case axis of
                        X -> [Front, Up, Back, Down]
                        Y -> [Front, Left, Back, Right]
                        Z -> [Up, Right, Down, Left]

--Face names don't correspond to expected behavior when called outside of rotateCube.
--The rotation has more to do with axis and clockwise-ness than face.
rotatePieceByFace :: Face -> (Piece -> Piece)
rotatePieceByFace face
    | face == Front = map (nextSticker zClockwise)
    | face == Right = map (nextSticker xClockwise)
    | face == Back = map (nextSticker $ reverse zClockwise)
    | face == Left = map (nextSticker $ reverse xClockwise)
    | face == Up = map (nextSticker yClockwise)
    | face == Down = map (nextSticker $ reverse yClockwise)
    where xClockwise = [Front, Up, Back, Down]
          yClockwise = [Front, Left, Back, Right]
          zClockwise = [Up, Right, Down, Left]

nextSticker :: [Face] -> Sticker -> Sticker
nextSticker sequence (face, color) = (nextInCycle sequence face, color)

--There may well be a cleaner/more concise way to write this
nextInCycle :: (Eq a) => [a] -> a -> a
nextInCycle sequence currentValue
    | isJust currentIndex = thisCycle !! (fromJust currentIndex + 1)
    | otherwise = currentValue
    where currentIndex = elemIndex currentValue sequence
          thisCycle = cycle sequence
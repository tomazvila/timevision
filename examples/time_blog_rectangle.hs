#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , reanimate
            , reanimate-svg
-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Graphics.SvgTree.Types
import Reanimate.Svg.Constructors
import Codec.Picture.Types

-- Time
day = hour * 24
hour :: Int
hour = 60
type Minutes = Int
data DayRect = DayRect Minutes Colour
oneUnitx = 16 / (fromIntegral day)
oneUnity = 60 * oneUnitx * (9 / 16)
-- Time


-- colours
type Colour = Tree -> Tree
black :: Colour
black = withFillColor "black"

red :: Colour
red = withFillColor "red"

blue :: Colour
blue = withFillColor "blue"

gray :: Colour
gray = withFillColorPixel $ PixelRGBA8 100 100 100 1

green :: Colour
green = withFillColor "green"

yellow :: Colour
yellow = withFillColor "yellow"

magenta :: Colour
magenta = withFillColor "magenta"

coral :: Colour
coral = withFillColor "coral"

darkSlateGray :: Colour
darkSlateGray = withFillColor "darkSlateGray"
-- colours


-- draw code
type Move = Double
type PrevMove = Double

dayRect :: DayRect -> Move -> Tree
dayRect (DayRect minutes colour) m =
    move scaled
  $ colour
  $ mkRect (fromIntegral minutes * oneUnitx) (oneUnity * 4)
    where
      scaled = m

rectMin :: DayRect -> Int
rectMin (DayRect m c) = m

blackRect :: Minutes -> DayRect
blackRect minutes = DayRect minutes black

move :: Double -> Tree -> Tree
move d t = translate d 0 t

drawDay :: [DayRect] -> Tree
drawDay rects = mkGroup $ go rects 0 0
  where
    nextMove :: DayRect -> Move
    nextMove (DayRect minutes _) = (fromIntegral minutes * oneUnitx) / 2

    go :: [DayRect] -> Double -> PrevMove -> [Tree]
    go [] _ _ = []
    go (x:xs) prevMove acc = dayRect x currentMove : go xs currentMove (nextMove x)
      where
        currentMove = acc + prevMove + nextMove x

moveToTheStart :: Tree -> Tree
moveToTheStart t = translate (-12 * (fromIntegral hour) * oneUnitx) 0 t

drawDay' :: [DayRect] -> Tree
drawDay' rects = drawDay $ fillMissing rects
-- draw code


-- Rectangles
sleep :: DayRect
sleep = DayRect (8 * hour) gray

prepare :: DayRect
prepare = DayRect 20 yellow

prepare' :: DayRect
prepare' = DayRect (3 * 60) yellow

commute :: DayRect
commute = DayRect 20 darkSlateGray 

eat :: DayRect
eat = DayRect 20 coral

work :: DayRect
work = DayRect (8 * hour + 30) blue 

rest :: DayRect
rest = DayRect (2 * hour) green 

freeTime :: DayRect
freeTime = DayRect (80 + hour + 20) magenta 

freeTime' :: DayRect
freeTime' = DayRect (1 * hour + 30) magenta 

freeTime'' :: DayRect
freeTime'' = DayRect (4 * hour) magenta 

freeTime''' :: DayRect
freeTime''' = DayRect (3 * hour) magenta 

fakeFreeTime = DayRect 251 magenta 

workout :: DayRect
workout = DayRect hour red 

workout' :: DayRect
workout' = DayRect 30 red 

tuesdayThursday :: [DayRect]
tuesdayThursday = [ sleep, workout', prepare, eat, commute, work, commute, commute, prepare, eat, rest, freeTime ]

weekend :: [DayRect]
weekend = [ sleep, workout', prepare, eat, freeTime', prepare', prepare, eat, freeTime'', prepare, eat, rest, freeTime''']

fakeWholeWeek :: [DayRect]
fakeWholeWeek = [ sleep, fakeFreeTime ]


fillMissing :: [DayRect] -> [DayRect]
fillMissing xs = reverse $ missingRect : (reverse xs)
  where
    total = sum $ fmap rectMin xs
    missing = max (day - total) 0
    missingRect = blackRect missing

main :: IO ()
main = reanimate
     $ staticFrame 1
     $ moveToTheStart
     $ drawDay' fakeWholeWeek



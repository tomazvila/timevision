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
import Data.List.Extra

type CircleSize = Double
type From = Int
type To = Int
type Colour = Tree -> Tree

weeks = 52
years = 80

weeks' = 52.0
years' = 80.0

nOfPercentageOfLife :: Double -> Int
nOfPercentageOfLife n = round $ (fromIntegral (weeks * years) / 100) * n

listOfCircles :: Int -> CircleSize -> [Tree]
listOfCircles n r = replicate n $ scale 0.05 $ mkCircle r

matrixFromList :: Int -> [Tree] -> [[Tree]]
matrixFromList x l = chunksOf x l

gridLayout' :: [[Tree]] -> Tree
gridLayout' rows = mkGroup
    [ translate (-screenWidth'/2+colSep*nCol + colSep*0.5)
                (screenHeight'/2-rowSep*nRow - rowSep*0.5)
      elt
    | (nRow, row) <- zip [0..] rows
    , let nCols = length row
          colSep = screenWidth' / fromIntegral nCols
    , (nCol, elt) <- zip [0..] row ]
  where
    screenHeight' = screenHeight 
    screenWidth' = screenHeight * (weeks'/years')
    rowSep = screenHeight' / fromIntegral nRows
    nRows = length rows

yellow :: Colour
yellow = withFillColor "yellow"

red :: Colour
red = withFillColor "red"

blue :: Colour
blue = withFillColor "blue"

gray :: Colour
gray = withFillColor "gray"

applyFnFromTo :: From -> To -> (a -> a) -> [a] -> [a]
applyFnFromTo f t fn l = beggining ++ (map fn middle) ++ end
  where
    beggining = take f l
    middle    = take (t - f) $ drop f l
    end       = drop t l 

data Colouring = Colouring From To Colour

colourFromTo :: Colouring -> [Tree] -> [Tree]
colourFromTo (Colouring f t c) circles = applyFnFromTo f t c circles

paintCircles :: [Colouring] -> [Tree] -> [Tree]
paintCircles colourings circles = foldl (\acc fn -> fn acc) initialValue functions 
  where 
    functions = map colourFromTo colourings 
    initialValue = circles

paintedCircles = paintCircles colours $ listOfCircles (weeks * years) 1 
  where
    timeGone = 1352 
    grayColouring = Colouring 0 timeGone gray
    redColouring = Colouring timeGone (timeGone + (11 * 52)) red
    yellowColouring = Colouring timeGone (timeGone + (36 * 52)) yellow
    colours = [ yellowColouring, redColouring ]

main :: IO ()
main = reanimate
     $ staticFrame 1
     $ scale 0.8
     $ gridLayout' $ matrixFromList weeks paintedCircles


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

elemsInMatrix :: [[a]] -> Int
elemsInMatrix = sum . map length

elemsToIterate :: Double -> [[a]] -> Int
elemsToIterate n m = round $ (fromIntegral (elemsInMatrix m) / 100) * n

mapPercentageOfElems :: (a -> a) -> Double -> [[a]] -> [[a]]
mapPercentageOfElems f n m = mapNOfElems f (elemsToIterate n m) m

nOfPercentageOfLife :: Double -> Int
nOfPercentageOfLife n = round $ (fromIntegral (weeks * years) / 100) * n

mapNOfElems :: (a -> a) -> Int -> [[a]] -> [[a]]
mapNOfElems f n m = go f n m 
  where
    singleLine :: (a -> a) -> Int -> [a] -> [a]
    singleLine f n l = case splitAt n l of
      (a, b) -> (map f a) ++ b 

    go :: (a -> a) -> Int -> [[a]] -> [[a]]
    go f n [] = []
    go f n (head:tail) = mappedHead : go f newN tail
      where
        headLength = length head
        howMuchToApply = if n > headLength then headLength else n
        newN = max 0 (n - headLength)
        mappedHead = singleLine f howMuchToApply head 

listOfCircles :: Int -> CircleSize -> [Tree]
listOfCircles n r = replicate n $ scale 0.05 $ mkCircle r

matrixOfCircles :: Int -> Int -> CircleSize -> [[Tree]]
matrixOfCircles x y r = replicate y $ listOfCircles x r

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

colourFromTo :: Colour -> From -> To -> [Tree] -> [Tree]
colourFromTo c f t l = beggining ++ (map c middle) ++ end
  where
    beggining = take f l
    middle    = case splitAt t l of (a, b) -> take f a
    end       = drop t l 

data Colouring = Colouring From To Colour

colourFromTo' :: Colouring -> [Tree] -> [Tree]
colourFromTo' (Colouring f t c) circles = colourFromTo c f t circles

paintCircles :: [Colouring] -> [Tree] -> [Tree]
paintCircles colourings circles = foldl (\acc fn -> fn acc) initialValue functions 
  where 
    functions = map colourFromTo' colourings 
    initialValue = circles

circles' = paintCircles colours $ listOfCircles (weeks * years) 1 
  where
    quarterOfLife = nOfPercentageOfLife 25
    yellowColouring = Colouring 0 quarterOfLife yellow
    redColouring = Colouring quarterOfLife (quarterOfLife * 2) red
    colours = [ yellowColouring ]

main :: IO ()
main = reanimate
     $ staticFrame 1
     $ scale 0.8
     $ gridLayout' $ matrixFromList weeks circles'


module Lib where

import Data.Ord
import Data.List (sortOn)  
import System.Random
 
-- https://en.wikipedia.org/wiki/Cutting_stock_problem

--           len,    width
type Rect = (Double, Double)

len :: Rect -> Double
len = fst

width :: Rect -> Double
width = snd

size :: Rect -> Double
size rect = width rect * len rect

weightedLength :: Rect -> Double
weightedLength (len, wid) = len^2 * wid

weightedWidth :: Rect -> Double
weightedWidth  (len, wid) = len * wid^2

ignore :: Rect -> Double
ignore _ = 1

arrange :: Double -> [Rect] -> [[Rect]]
arrange _        []    = [[]]
arrange maxWidth rects = let (rest, row) = fillRow maxWidth (rects, [])
                          in if null rest 
                                then [row]
                                else row : arrange maxWidth rest

fillRow :: Double -> ([Rect], [Rect]) -> ([Rect], [Rect])
fillRow _ current@([], _) = current
fillRow maxWidth current@(x:xs, row) = 
    if (width x) + (totalWidth row) <= maxWidth
    then fillRow maxWidth (xs, row ++ [x])
    else current

totalWidth :: [Rect] -> Double
totalWidth = foldr ((+) . width) 0

totalLength :: [Rect] -> Double
totalLength = foldr ((+) . len) 0

maxLength :: [Rect] -> Double
maxLength = maximum . map len

rects :: [Rect]
rects = [(100,60),(120,60),(80,40),(120,40)]

type WeightFunction = Rect -> Double

arrangeWith :: Double-> [Rect] -> WeightFunction -> [[Rect]]
arrangeWith maxWidth rects weightFun=
    let preorderedRects = sortOn (Down . weightFun) rects
     in arrange maxWidth preorderedRects

weightFunctions :: [Rect -> Double]
weightFunctions = [len,width,size,weightedLength,weightedWidth,ignore]

arrangeWithAll :: [WeightFunction] -> Double -> [Rect] -> [[Rect]]   
arrangeWithAll allFuns maxWidth rects =
    let allTrials      = map (arrangeWith maxWidth rects) allFuns 
        weightedTrials = map (\l -> (l, sum (map maxLength l))) allTrials
        sortedTrials   = sortOn snd weightedTrials
     in fst $ head sortedTrials
     
randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]     

rndDouble :: IO Double
rndDouble = randomIO
     
test :: IO ()
test = do
  seed <- randomIO :: IO Int
  let maxWidth       = 100
      
      allTrials      = map (arrangeWith maxWidth rects) weightFunctions
      weightedTrials = map (\l -> (l, sum (map maxLength l))) allTrials
      sortedTrials   = sortOn snd weightedTrials
  
  --let res = arrangeWithAll weightFunctions 100 rects
  print sortedTrials

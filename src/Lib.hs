module Lib where

import           Data.List     (sortOn, nub, group, sort)
import           Data.Ord
import           System.Random

-- https://en.wikipedia.org/wiki/Cutting_stock_problem

--           len,    width
type Rect = (Double, Double)

type WeightFunction = Rect -> Double

len :: WeightFunction
len = fst

width :: WeightFunction
width = snd

size :: WeightFunction
size = uncurry (*)

sizeTimesLength :: WeightFunction
sizeTimesLength (l, w) = l ^ 2 * w

sizeTimesWidth :: WeightFunction
sizeTimesWidth (l, w) = l * w ^ 2

ignore :: WeightFunction
ignore _ = 1

rndWeight :: WeightFunction
rndWeight (l, w) = frac $ 1237 * l + 4789 * w

frac :: Double -> Double
frac x = x - fromInteger (floor x)

arrange :: Double -> [Rect] -> [[Rect]]
arrange _ [] = [[]]
arrange maxWidth rects =
  let (rest, row) = fillRow maxWidth (rects, [])
   in if null rest
        then [row]
        else row : arrange maxWidth rest

fillRow :: Double -> ([Rect], [Rect]) -> ([Rect], [Rect])
fillRow _ current@([], _) = current
fillRow maxWidth current@(x : xs, row) =
  if width x + totalWidth row <= maxWidth
    then fillRow maxWidth (xs, row ++ [x])
    else current

totalWidth :: [Rect] -> Double
totalWidth = foldr ((+) . width) 0

totalLength :: [Rect] -> Double
totalLength = foldr ((+) . len) 0

maxLength :: [Rect] -> Double
maxLength = maximum . map len

rects :: [Rect]
rects = [(100, 60), (120, 60), (80, 40), (120, 40)]

arrangeWith :: Double -> [Rect] -> WeightFunction -> [[Rect]]
arrangeWith maxWidth rects weightFun =
  let preorderedRects = sortOn (Down . weightFun) rects
   in arrange maxWidth preorderedRects

weightFunctions :: [WeightFunction]
weightFunctions = [len, width, size, sizeTimesLength, sizeTimesWidth, ignore, rndWeight]

weightFunctionNames :: [String]
weightFunctionNames = ["length","width","size","sizeTimesLenth","sizeTimesWidth","initial","random"]

arrangeWithAll :: [WeightFunction] -> Double -> [Rect] -> [[Rect]]
arrangeWithAll allFuns maxWidth rects =
  let allTrials = map (arrangeWith maxWidth rects) allFuns
      weightedTrials = map (\l -> (l, sum (map maxLength l))) allTrials
      sortedTrials = sortOn snd weightedTrials
   in fst $ head sortedTrials

digits :: Int -> Double -> Double
digits n d = fromInteger (round (d * 10^n)) / 10^n

rndDouble :: IO Double
rndDouble =  digits 4 <$> randomIO

rndRect :: IO Rect
rndRect = do
  l <- rndDouble
  w <- rndDouble
  return (l, w)

toCSV :: (Num a, Show a) => [([Rect], a)] -> String
toCSV [] = ""
toCSV ((rectList, len) : xs) = toCSVRects rectList ++ show len ++ "\n" ++ toCSV xs
  where
    toCSVRects :: [Rect] -> String
    toCSVRects []            = ""
    toCSVRects ((l, w) : xs) = show l ++ separator ++ show w ++ separator ++ toCSVRects xs

    separator :: String
    separator = ","

markFirst :: [a] -> [(a, Int)]
markFirst l =
  let attr = 1 : [0,0..]
   in zip l attr

prepareForMlOutput :: [Rect] -> [([Rect], Int)]
prepareForMlOutput rects =
  let maxWidth = 1
      allSorts  = map (\wf -> sortOn (Down . wf) rects) weightFunctions
      allTrials = map (arrangeWith maxWidth rects) weightFunctions
      sortsAndTrials = zip allSorts allTrials
      weightedSorts = map (\(sorting,l) -> (sorting, sum (map maxLength l))) sortsAndTrials
      sortedSorts = sortOn snd weightedSorts
      sortedLists = nub $ map fst sortedSorts
   in markFirst sortedLists

prepareStatistics :: [Rect] -> String
prepareStatistics rects =
  let maxWidth = 1
      --allSorts  = map (\wf -> sortOn (Down . wf) rects) weightFunctions
      allTrials = map (arrangeWith maxWidth rects) weightFunctions
      weightsAndTrials = zip weightFunctionNames allTrials
      weightedSorts = map (\(weight,l) -> (weight, sum (map maxLength l))) weightsAndTrials
      sortedSorts = sortOn snd weightedSorts
   in fst . head $ sortedSorts

genRects :: Int -> IO [Rect]
genRects n  = mapM (const rndRect) [1 .. n]

test :: IO ()
test = do
  let numRects = 20
      numRuns  = 10000
      header   = concatMap (\i -> "R_" ++ show i ++ "_len,R_" ++ show i ++ "_wid,") [1..numRects]  ++ "Optimum\n"
  loloRects <- mapM (const $ genRects numRects) [1..numRuns]
  writeFile "data.csv" $ header ++ concatMap (toCSV . prepareForMlOutput) loloRects
  
  --writeFile "stats.csv" $ concatMap (\l -> show (prepareStatistics l) ++ "\n") loloRects
  --print $ map prepareStatistics loloRects
  print $ map (\l -> (head l, length l)) $ group $ sort $ map prepareStatistics loloRects

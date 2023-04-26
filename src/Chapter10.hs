{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Chapter10
  ( solveRPN,
    pathLen,
    heathrowToLondon,
    roadStep,
    optimalPath,
    groupsOf,
    roadsCalcMain,
  )
where

solveRPN :: (Floating a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction (x : xs) "ln" = log x : xs
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numStr = read numStr : xs

data Section = Section
  { getA :: Int,
    getB :: Int,
    getC :: Int
  }
  deriving (Show)

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

pathLen :: Path -> Int
pathLen = sum . map snd

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let pathLenA = pathLen pathA
      pathLenB = pathLen pathB
      nextPathA =
        if a + pathLenA <= b + c + pathLenB
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      nextPathB =
        if b + pathLenB <= a + c + pathLenA
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (nextPathA, nextPathB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestPathA, bestPathB) = foldl roadStep ([], []) roadSystem
   in if pathLen bestPathA <= pathLen bestPathB
        then reverse bestPathA
        else reverse bestPathB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

roadsCalcMain :: IO ()
roadsCalcMain = do
  contents <- getContents
  let nums = map read $ lines contents :: [Int]
      roadSystem = map (\case [a, b, c] -> Section a b c) $ groupsOf 3 nums :: RoadSystem
      thePath = optimalPath roadSystem
  putStrLn $ "the shortest path is:" ++ concatMap (show . fst) thePath
  putStrLn $ "the length is:" ++ show (sum $ map snd thePath)
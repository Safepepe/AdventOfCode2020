module Problem3 where

type Right = Int
type Down = Int
type Slope = (Right , Down)
type Map = [String]

exampleSlopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

readInput :: IO [String]
readInput = do
  str <- readFile "data3.txt"
  return $ lines str

takePosition ::  Int -> String -> Char
takePosition  position  str = str!!(mod position (length str) )

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = map snd $ filter zeroModN $ zip [0..(length xs -1)] xs
  where zeroModN (y, _) = mod y n == 0

trail :: Map -> Slope -> String
trail strs (right, down) =  foldr savePosition [] flatTaggedMap
  where flatMap = takeEvery down strs
        positions = map (*right) [0.. (length flatMap -1)]
        flatTaggedMap = zip positions flatMap
        savePosition (position, row) pastps = (takePosition position row):pastps

howManyTrees :: Map -> Slope -> Int
howManyTrees  mp slp = length . filter (=='#') $ trail mp slp

answer = do
  chart <- readInput
  let treeNumbers  = map (howManyTrees chart) exampleSlopes
  print treeNumbers
  print $ product treeNumbers

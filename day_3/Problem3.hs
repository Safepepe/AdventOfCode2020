module Problem3 where
import Data3 (inputIO)

{-Part One -}
type Right = Int
type Down = Int
type Slope = (Right , Down)
type Map = [String]

slope1= (3,1)

position ::  Int -> String -> Char
position  position  str = str!!(mod position (length str) )

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = (xs!!) <$> postns
  where postns = if n <= length xs then [0,n..(length xs -1)] else [0]

trail :: Map -> Slope -> String
trail lns (right, down) =  zipWith position xCoords rowsVisited
  where rowsVisited = takeEvery down lns
        xCoords  = (*right) <$> [0..(length rowsVisited -1)]

howManyTrees :: Map -> Slope -> Int
howManyTrees  mp slp = length . filter (=='#') $ trail mp slp

answer1 :: IO Int
answer1 = do
  chart <- inputIO
  let treeNumbers  = howManyTrees chart slope1
  return treeNumbers

{-Part Two -}
exampleSlopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

answer2 :: IO Int
answer2 = do
  chart <- inputIO
  let treeProduct  = product $ (howManyTrees chart) <$> exampleSlopes
  return treeProduct

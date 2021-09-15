module Problem1 where
import Data1
--Part 1.
{-Find THE two entries that sum to 2020 and then multiply those two numbers together.-}
sumTo :: Int -> Int -> Int -> Bool
sumTo n x y = (x + y) == n

sumTo2020 :: Int -> Int -> Bool
sumTo2020 = sumTo 2020

pairWith :: Int -> [Int] -> [(Int,Int)]
pairWith n ns = map (\x -> (x,n)) (filter (sumTo2020 n) ns)

getSumPairs :: [Int] -> [(Int,Int)]
getSumPairs [] = []
getSumPairs (n:ns) = (pairWith n ns) ++ (getSumPairs ns)

---Part 2
--Find the three entries that sum to 2020--
total = 2020

tupleSumToTotal :: Int -> Int -> [Int] -> [(Int,Int,Int)]
tupleSumToTotal p n = map (\x -> (p, n, x)) $ filter (sumsTo (total - p) n)

answer :: [Int] -> [(Int,Int,Int)]
answer  = concat . map tupleUp . zipToRest
  where
    zipToRest []     = []
    zipToRest (x:xs) = (x,xs) : (zipToRest xs)
    tupleUp (x, [])    = []
    tupleUp (x, (y:ys)) = tupleSumToTotal x y ys ++ (tupleUp (x, ys))

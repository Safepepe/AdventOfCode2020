module Problem1 where
import Data1 (inputIO)
--Part 1.
{-Find THE two entries that sum to 2020 and then multiply those two numbers together.-}

sumTo :: Int -> Int -> Int -> Bool
sumTo n x y = (x + y) == n

pairWith :: Int -> [Int] -> [(Int,Int)]
pairWith n ns = map (\x -> (x,n)) (filter (sumTo 2020 n) ns)

getSumPairs :: [Int] -> [(Int,Int)]
getSumPairs [] = []
getSumPairs (n:ns) = (pairWith n ns) ++ (getSumPairs ns)

answer0 :: IO Int
answer0 = do
  input <- inputIO
  return $ head $ multiply <$> (getSumPairs input)
   where multiply (x,y) = x*y

---Part 2
--Find the three entries that sum to 2020--
total = 2020

tupleSumToTotal :: Int -> Int -> [Int] -> [(Int,Int,Int)]
tupleSumToTotal p n = map (\x -> (p, n, x)) . filter (sumTo (total - p) n)

answer :: [Int] -> [(Int,Int,Int)]
answer  = concat . map tupleUp . zipToRest
  where
    zipToRest []     = []
    zipToRest (x:xs) = (x,xs) : (zipToRest xs)
    tupleUp (x, [])    = []
    tupleUp (x, (y:ys)) = tupleSumToTotal x y ys ++ (tupleUp (x, ys))

answer1 :: IO Int
answer1 = do
  input <- inputIO
  return $ head $ multiply3 <$> answer input
    where multiply3 (x,y,z) = x*y*z

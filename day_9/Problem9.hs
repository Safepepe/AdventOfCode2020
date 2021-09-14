module Problem9 where
import Control.Monad.State.Lazy


--I'll just brute force it... this will be n^2
sumsTo :: [Int] -> Int -> Bool
sumsTo xs n = n `elem` results
  where pairs     = [(x,y)| x<-xs, y<-xs]
        diffPairs = filter (\(x,y) -> x /= y) pairs
        results = map (\(x,y)-> x+y) diffPairs

type FirstToFail = State (Bool, Position)
type Position = Int

preambleSize = 25
initState = (False, preambleSize) --we start with the number after the preamble

firstToFail :: [Int] -> FirstToFail [Int]
firstToFail numList = do (cond, pos) <- get
                         if pos >= length numList then
                          return numList
                         else
                          do let preamble = take preambleSize . drop (pos-preambleSize) $ numList
                                 currentN = numList !! pos
                             if preamble `sumsTo` currentN then
                              do put $ (False, pos +1)
                                 firstToFail numList
                             else
                              do put $ (True, pos)
                                 return numList

{-Answer to the first part of the problem-}
input :: IO [Int]
input = readFile "data9.txt" >>= return . map read . lines

answer :: IO Int
answer = do numList <- input
            let pos = snd $ execState (firstToFail numList) initState
            return $ numList!!pos
{-For the next part of the problem we are also going to brute force it-}
type Length = Int

listsOfSize :: Length -> [Int] -> [[Int]]
listsOfSize n numList = trim listOfLists
  where listOfLists = zipWith ($) (map drop [0..n-1]) $ replicate n numList
        trim xs  = map (take $ length $ last xs) xs

listsAddingTo :: Int -> [Int] -> Length -> [[Int]]
listsAddingTo value numList size = filter ((== value) .sum) listOfLists
      where listOfLists = listsOfSize size numList

answer2 :: IO Int
answer2 = do numList <- input
             value  <- answer
             let maxLength = length numList -1
                 sizes     = [2..maxLength]
                 lists     = concat $map (listsAddingTo value numList) sizes
                 ans       = head lists
             return $ head ans + last ans

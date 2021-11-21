module Problem9 where
import Data9 (inputIO)
import Control.Monad.State.Lazy
import GHC.Exts (sortWith)
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type FirstToFail = State (Bool, Position)
type Position = Int

preambleSize = 25
initState = (False, preambleSize) --we start at the position preambleSize

pairSumsTo :: [Int] -> Int -> Bool
pairSumsTo xs n = foldr substractAndLook False pairedXs
  where
    pairedXs = map (\(y,ys) -> (y, filter (/=y) ys)) . zip xs $ tails xs
    tails ys = if null ys then [] else tail ys : tails (tail ys)
    substractAndLook:: (Int,[Int]) -> Bool -> Bool
    substractAndLook (m,ms) bl = (n-m)`elem`ms || bl

firstToFail :: [Int] -> FirstToFail [Int]
firstToFail numList =
  do
    (cond, pos) <- get
    if pos >= length numList then -- we have arrived at the end
      return numList
    else
      do
        let preamble = take preambleSize . drop (pos-preambleSize) $ numList
            currentN = numList !! pos
        if preamble `pairSumsTo` currentN then
          do
            put (False, pos +1)
            firstToFail numList
        else
          do
            put (True, pos)
            return numList

{-=---------------------------=-}
answer1 :: IO Int
answer1 = do numList <- inputIO
             let pos = snd $ execState (firstToFail numList) initState
             return $ numList!!pos
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
-- Brute-forcing it.
type Length = Int

subintervals :: [a] -> [[a]]
subintervals = sortWith (length). concat . map inits . tails
 where
   tails :: [a] -> [[a]]
   tails [] = []
   tails xs = xs : tails (tail xs)
   inits :: [a] -> [[a]]
   inits [] = []
   inits xs = xs : inits (init xs)

intervalsAddingTo :: Int -> [Int] -> [[Int]]
intervalsAddingTo value nList = filter ((== value) .sum) $ intervals
  where
    n = length nList
    intervals = drop n . subintervals$ nList --we don't care for singletons

answer2 :: IO Int
answer2 = do numList <- inputIO
             value  <- answer1
             let lists     = intervalsAddingTo value numList
                 ans       = head lists
             return $ head ans + last ans

module Problem10 where
import Control.Monad.State.Lazy
import Data.List

sortedInput:: IO [Int]
sortedInput = readFile "data10.txt" >>= return . (0:) .addPcAdaptor.  map read . lines

addPcAdaptor :: [Int] -> [Int]
addPcAdaptor xs = sort $ (maxAdaptor +3):xs
  where maxAdaptor = foldr max 0 xs
--we assume that our list is sorted.
getDistro :: [Int] -> (Int,(Int,Int,Int,Int))
getDistro []  = (-3,(0,0,0,0))
getDistro [x] = (-3,(0,0,0,0))
getDistro xs  = foldr counter (0,(0,0,0,0)) . reverse $ xs
 where counter next (past,(d0,d1,d2,d3)) = case next - past of
                                       3 -> (next, (d0,d1,d2,d3+1))
                                       2 -> (next, (d0,d1,d2+1,d3))
                                       1 -> (next, (d0,d1+1,d2,d3))
                                       0 -> (next, (d0+1,d1,d2,d3))
                                       _ -> (-3, (d0,d1,d2,d3))


{-Part 2 of the problem-}
{-We try to solve a different problem -}

type Pair = (Int, [Int]) --(result  of the list to the right, list)
type Answer = State (Pair,Pair,Pair)


--we assume that the adaptor is "admissible"
{- we take a decreasing list of adaptors. Suppose that we have all the pertinent
information for that list. Now take a new lesser adaptor a0 and add it to the list.
What is the answer we want but for this new list ? -}
addLesserAdaptor :: Int -> Answer ()
addLesserAdaptor a0 = do (p1,p2,p3) <- get
                         let cond1 = condition a0 p1
                             cond2 = condition a0 p2
                             cond3 = condition a0 p3
                         if cond3 then
                           put ( ((fst p1)+(fst p2)+(fst p3), a0:(snd p1)), p1, p2)
                         else
                           if cond2 then
                             put ( ((fst p1)+(fst p2), a0:(snd p1)), p1 , p2 )
                           else
                             put ( ((fst p1), a0:(snd p1)), p1 , p2 )
    where condition :: Int -> Pair -> Bool
          condition a0 (val, [])     = True
          condition a0 (val, (a:as)) = a - a0 <= 3

baseCase :: (Int,Int,Int) -> (Pair,Pair,Pair)
baseCase (an,an1,an2)
     | an - an2 <= 3 = ((2,[an2,an1,an]),(1,[an1,an]),(1,[an]))
     | otherwise     = ((1,[an2,an1,an]),(1,[an1,an]),(1,[an]))

toTuple :: [Int] -> (Int,Int,Int)
toTuple (x1:x2:x3:xs) = (x1,x2,x3)

answer2 :: IO Int
answer2 = sortedInput >>= (\increasingList ->
          do let downList  = reverse $ increasingList
                 initState = baseCase . toTuple . take 3 $ downList
                 rest      = drop 3 downList
                 (p1,p2,p3)= execState (mapM_ addLesserAdaptor rest) initState
             return $ fst p1       )

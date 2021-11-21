module Problem10 where
import Data10 (inputIO)
import Control.Monad.State.Lazy
import Data.List

{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
sortedInput:: IO [Int]
sortedInput = inputIO >>= return.(0:).addPcAdaptor
  where
    addPcAdaptor :: [Int] -> [Int]
    addPcAdaptor xs = let maxAdaptor = foldr max 0 xs in sort $ (maxAdaptor +3):xs

--we assume that our list is sorted.
getDistro :: [Int] -> (Maybe Int,(Int,Int,Int,Int))
getDistro []  = (Nothing,(0,0,0,0))
getDistro [x] = (Nothing,(0,0,0,0))
getDistro xs  = foldr countJumps (Just 0,(0,0,0,0)) . reverse $ xs
 where
   countJumps _    (Nothing, (d0,d1,d2,d3))  = (Nothing, (d0,d1,d2,d3))
   countJumps next (Just past,(d0,d1,d2,d3)) = case next - past of
                                       3 -> (Just next, (d0,d1,d2,d3+1))
                                       2 -> (Just next, (d0,d1,d2+1,d3))
                                       1 -> (Just next, (d0,d1+1,d2,d3))
                                       0 -> (Just next, (d0+1,d1,d2,d3))
                                       _ -> (Nothing, (d0,d1,d2,d3))
{-=--------------------=-}
answer1 :: IO (Maybe Int)
answer1 = do
  adapters <- sortedInput
  let (pc, (d0,d1,d2,d3)) = getDistro adapters
  if pc == Nothing then
    return Nothing
  else
    return $ Just (d1*d3)
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
type NumberOfArrangements = Int
type Adaptor = Int
type Pair = (NumberOfArrangements, [Adaptor])
type Answer = State (Pair,Pair,Pair)

addLesserAdaptor :: Adaptor -> Answer ()
addLesserAdaptor a0 =
  do
    (p1,p2,p3) <- get
    let cond1 = a0`canBeAddedTo`p1
        cond2 = a0`canBeAddedTo`p2
        cond3 = a0`canBeAddedTo`p3
    if cond3 then
      put ((fst p1 + fst p2 + fst p3, a0 : snd p1), p1, p2)
    else
      if cond2 then
        put ((fst p1 + fst p2 , a0 : snd p1), p1 , p2)
      else
        put ((fst p1, a0 : snd p1), p1 , p2)
  where
    canBeAddedTo :: Adaptor -> Pair -> Bool
    canBeAddedTo a0 (val, [])     = True
    canBeAddedTo a0 (val, (a:as)) = a - a0 <= 3

{-=--------------------=-}
answer2 :: IO Int
answer2 = do decreasingAdaptorList <- (sortedInput >>= return.reverse)
             let [pcAdptr, adptr2, adptr1] = (decreasingAdaptorList!!)<$>[0,1,2]
                 initState = baseCase (pcAdptr, adptr2, adptr1)
                 rest      = drop 3 decreasingAdaptorList
                 (pair1,pair2,pair3) = execState (mapM_ addLesserAdaptor rest) initState
             return $ fst pair1
   where
     baseCase :: (Int,Int,Int) -> (Pair,Pair,Pair)
     baseCase (a3,a2,a1) --We assume a3 > a2 > a1
          | a3 - a1 <= 3 = ((2,[a1,a2,a3]),(1,[a2,a3]),(1,[a3]))
          | otherwise    = ((1,[a1,a2,a3]),(1,[a2,a3]),(1,[a3]))

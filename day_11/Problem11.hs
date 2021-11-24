module Problem11 where
import Data11 (inputIO)
import Control.Monad.State.Lazy
import GHC.Exts(sortWith)
import Data.List(groupBy)

type Row = String
type Chart = [Row]
type Position = (Int,Int)
type ChartState = State Chart


{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
{-
Rules:
   1. If a seat is empty (L) and there are no occupied seats adjacent to it,
      the seat becomes occupied.
   2. If a seat is occupied (#) and four or more seats adjacent to it are also
      occupied, the seat becomes empty.
   3. Otherwise, the seat's state does not change.
-}
floorCell    = '.'
emptyCell    = 'L'
occupiedCell = '#'

cell :: Position -> ChartState (Maybe Char)
cell (x,y) = do
             chrt <- get
             let xMax = (length.head$chrt) - 1
                 yMax = length chrt - 1
             if (x >= 0) && (x <= xMax) && (y>= 0) && (y <= yMax) then
               return $ Just ((chrt!!y)!!x)
             else
               return Nothing

isEmpty :: Position -> ChartState Bool
isEmpty pos = do
              chr <- cell pos
              return $ chr == (Just emptyCell)

isOccupied :: Position -> ChartState Bool
isOccupied pos = do chr <- cell pos
                    return $ chr == (Just occupiedCell)

adjecentCells :: Position -> ChartState [Char]
adjecentCells (x,y) = mapM cell adjecents >>= return.deJust.sequence.filter (/= Nothing)
 where
   adjecents :: [Position]
   adjecents = filter (/= (x,y)) $ (,)<$>[x-1,x,x+1]<*>[y-1,y,y+1]
   deJust :: Maybe [a] -> [a]
   deJust = foldr (const) [] -- deJust (Just xs) = xs |<-->| deJust Nothing = []

ruleOne :: Position -> ChartState Bool
ruleOne pos = do
  freeSeat <- isEmpty pos
  adjecents <- adjecentCells pos
  let noOneAdjecent = and$(/=occupiedCell)<$>adjecents
  if freeSeat && noOneAdjecent then
    return True
  else
    return False

ruleTwo :: Position -> ChartState Bool
ruleTwo pos = do
  occupiedSeat <- isOccupied pos
  adjecents <- adjecentCells pos
  let atleastFourOccupied =  (3 <).length$ filter (==occupiedCell) adjecents
  if occupiedSeat && atleastFourOccupied then
    return True
  else
    return False

willChange :: [(Position -> ChartState Bool)] -> Position -> ChartState Bool
willChange rules pos = do
                 bls <- sequence $rules<*>[pos]
                 return $ or bls

coordsChart :: ChartState [[Position]]
coordsChart = do chrt <- get
                 let xMax = length (head chrt) -1
                     yMax = length chrt -1
                     yAxis = [0..yMax]
                     xAxis = [0..xMax]
                 return $ map (zip xAxis . repeat) yAxis

changesChart :: [(Position -> ChartState Bool)] -> ChartState [[Bool]]
changesChart rules = do
               chrt <- get
               posChart <- coordsChart -- [[(x,y)]]
               changeMap <- mapM (mapM$willChange rules) posChart
               return changeMap

applyChanges :: [[Bool]] -> ChartState Chart
applyChanges chngsChrt = do
               chrtSt <-get
               return $ fuse chngsChrt chrtSt
    where
      fuse :: [[Bool]] -> Chart -> Chart
      fuse bls chrt = zipWith (\l1 l2 -> zipWith changeChar l1 l2) bls chrt
      changeChar :: Bool -> Char -> Char
      changeChar False ch = ch
      changeChar True 'L' = '#'
      changeChar True '#' = 'L'
      changeChar True _ = 'X'

recursiveChanges :: [(Position -> ChartState Bool)] -> ChartState ()
recursiveChanges rules = do
                   chrt <- get
                   boolChrt <- changesChart rules
                   let noChanges = not$or$or<$>boolChrt
                   if noChanges then
                     return ()
                   else
                     applyChanges boolChrt >>= put >> recursiveChanges rules

{-=----------------------------------=-}
answer1 :: IO Int
answer1 = do
         chrt <- inputIO
         let rules = [ruleOne, ruleTwo]
             endState = execState (recursiveChanges rules)  chrt
             numOfOccupiedSeats = length.filter (==occupiedCell).concat$endState
         return numOfOccupiedSeats
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
{-
Rules:
   1. If a seat is empty (L) and there are no occupied seats adjacent to it,
      the seat becomes occupied.
   2. If a seat is occupied (#) and four or more seats adjacent to it are also
      occupied, the seat becomes empty.
   3. Otherwise, the seat's state does not change.
-}
--we need to be able to look in 8 directions and recognize the first seat.

data Direction = UpDir | DownDir | RightDir | LeftDir | UpRightDir | UpLeftDir | DownRightDir| DownLeftDir
  deriving(Eq,Show)

eightDirs = [UpDir, DownDir, LeftDir, RightDir, UpLeftDir, UpRightDir, DownLeftDir, DownRightDir]

look :: Position -> Direction ->  ChartState [Position]
look (x,y) dir = do chrt <- get
                    let xMax = (length$head chrt) -1
                        yMax = length chrt - 1
                    case dir of
                      UpDir       -> return [(x  ,y-d)| d<-[1..y]]
                      DownDir     -> return [(x  ,y+d)| d<-[1..(yMax-y)]]
                      RightDir    -> return [(x+d,y  )| d<-[1..(xMax-x)]]
                      LeftDir     -> return [(x-d,y  )| d<-[1..x]]
                      UpRightDir  -> return [(x+d,y-d)| d<-[1..(min y (xMax-x))]]
                      UpLeftDir   -> return [(x-d,y-d)| d<-[1..(min y x)]]
                      DownLeftDir -> return [(x-d,y+d)| d<-[1..(min (yMax-y) x)]]
                      DownRightDir-> return [(x+d,y+d)| d<-[1..(min (yMax-y) (xMax-x))]]

firstSeat :: [Position] -> ChartState (Maybe Position, Maybe Char)
firstSeat []     = return (Nothing, Nothing)
firstSeat (p:ps) = do
                   ch <- cell p
                   if ch == Just floorCell then
                      firstSeat ps
                   else
                      return $ (Just p, ch)

lookForSeat :: Position -> Direction -> ChartState  (Maybe Char)
lookForSeat p dir = look p dir >>= firstSeat >>= return.snd

lookAround :: Position -> ChartState [Maybe Char]
lookAround p =  mapM (lookForSeat p) eightDirs

newRuleOne :: Position -> ChartState Bool
newRuleOne pos = do
  freeSeat <- isEmpty pos
  visibleSeats <- lookAround pos
  let noOneVisible = and.map(/=(Just occupiedCell)).filter(/= Nothing)$visibleSeats
  if freeSeat && noOneVisible then
    return True
  else
    return False

newRuleTwo :: Position -> ChartState Bool
newRuleTwo pos = do
  occupiedSeat <- isOccupied pos
  visibleSeats <- lookAround pos
  let atleastFourOccupied =  (4 <).length.filter(==(Just occupiedCell))$visibleSeats
  if occupiedSeat && atleastFourOccupied then
    return True
  else
    return False
{-=----------------------------------=-}
answer2 :: IO Int
answer2 = do
         chrt <- inputIO
         let rules = [newRuleOne, newRuleTwo]
             endState = execState (recursiveChanges rules)  chrt
             numOfOccupiedSeats = length.filter (==occupiedCell).concat$endState
         return numOfOccupiedSeats

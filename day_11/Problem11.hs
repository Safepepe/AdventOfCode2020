module Problem11 where
import Control.Monad.State.Lazy
import GHC.Exts(sortWith)
import Data.List(groupBy)
--Change the code below ot using the state monad !
--It shouldn't be difficult.

type Row = String
type Chart = [Row]
type Position = (Int,Int)
type ChartState = State Chart

input :: IO Chart
input =  readFile "data11.txt" >>= return . lines

floorCell    = '.'
emptyCell    = 'L'
occupiedCell = '#'

howManyOccupied :: Chart -> Int
howManyOccupied chrt = length . filter (==occupiedCell) $ concat chrt

answer2 :: IO Int
answer2 = input >>= return . howManyOccupied . execState loop

{-===================      Testing code      ===================-}
testOn :: IO Chart -> (a -> ChartState b) -> a -> IO (b,Chart)
testOn ioChrt f  a = ioChrt >>= return . runState (f a)


{-===================           Code         ===================-}

{-===================           Part2         ===================-}

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

firstSeat :: [Position] -> ChartState (Maybe Position)
firstSeat []     = return Nothing
firstSeat (p:ps) = do chrt  <- get
                      seat <- isSeat p
                      if seat then
                        return $ Just p
                      else
                        firstSeat ps

lookForSeat :: Position -> Direction -> ChartState (Direction, Maybe Position)
lookForSeat p dir = look p dir >>= firstSeat >>= (\x -> return (dir, x))

ruleOne :: Position -> ChartState Bool
ruleOne pos = do occupied <- isOccupied pos
                 seatsSeen <- mapM (lookForSeat pos) eightDirs
                 seatsOccupied <- filterM (isJustOccupied.snd) seatsSeen
                 if occupied && length seatsOccupied >= 5 then
                   return  True
                 else
                   return False
   where isJustOccupied Nothing  = return False
         isJustOccupied (Just p) = isOccupied p

ruleTwo :: Position -> ChartState Bool
ruleTwo pos = do empty <- isEmpty pos
                 seatsSeen <- mapM (lookForSeat pos) eightDirs
                 seatsNotOccupied <- filterM ((>>=return.not).isJustOccupied.snd) seatsSeen
                 if empty && length seatsNotOccupied == 8 then
                   return True
                 else
                   return False
   where isJustOccupied Nothing  = return False
         isJustOccupied (Just p) = isOccupied p

needsToChange :: Position -> ChartState Bool
needsToChange pos = do r1 <- ruleOne pos
                       r2 <- ruleTwo pos
                       if r1 || r2 then
                        return True
                       else
                        return False

allPositions2 :: ChartState [[Position]]
allPositions2 = do chrt <- get
                   let xMax = length (head chrt) -1
                       yMax = length chrt -1
                       yAxis = [0..yMax]
                       xAxis = [0..xMax]
                   return $ map (zip xAxis . repeat) yAxis

changesChart :: ChartState [[Bool]]
changesChart = do pos <- allPositions2
                  boolChart <- mapM (mapM needsToChange) pos
                  return boolChart

applyChanges :: [[Bool]] -> ChartState ()
applyChanges bools = do chrt <- get
                        put$ zipWith mergeRows bools chrt
  where mergeRows bs chs = zipWith mergeCell bs chs
        mergeCell True '#' = 'L'
        mergeCell True 'L' = '#'
        mergeCell False ch = ch

loop :: ChartState ()
loop = do boolChart <- changesChart
          let someChange = or . map or $ boolChart
          if someChange then
            do applyChanges boolChart
               loop
          else
            return ()


{-===================           Part 1         ===================-}
{- I think I solved this part in the most stupid and complicated way possible...


adjecentCells :: Position -> ChartState [Char]
adjecentCells (x,y) = do uprRow     <- upperRow (x,y)
                         leftRight <- leftRightCells (x,y)
                         lwrRow     <- lowerRow (x,y)
                         return $ uprRow ++ leftRight ++ lwrRow
-}
cell :: Position -> ChartState Char
cell (x,y) = do chrt <- get
                return $ (chrt!!y)!!x

isEmpty :: Position -> ChartState Bool
isEmpty pos = do chr <- cell pos
                 return $ chr == emptyCell

isOccupied :: Position -> ChartState Bool
isOccupied pos = do chr <- cell pos
                    return $ chr == occupiedCell

isSeat :: Position -> ChartState Bool
isSeat pos = do empty <- isEmpty pos
                occupied <- isOccupied pos
                return $ empty || occupied
{-
fourOrMoreOccupied :: Position -> ChartState Bool
fourOrMoreOccupied pos = do neighbors <- adjecentCells pos
                            return $ (>=4) . length . filter (=='#') $ neighbors

emptyAdjecentSeats :: Position -> ChartState Bool
emptyAdjecentSeats pos = do neighbors <- adjecentCells pos
                            return $ and . map (==emptyCell) . filter (/='.') $ neighbors

becomesOccupied :: Position -> ChartState Bool
becomesOccupied pos = do empty <- isEmpty pos
                         emptyneighbors <- emptyAdjecentSeats pos
                         return $ empty && emptyneighbors

becomesEmpty :: Position -> ChartState Bool
becomesEmpty pos = do occupied <- isOccupied pos
                      fourOrMoreNeighborsOccupied <- fourOrMoreOccupied pos
                      return $ occupied && fourOrMoreNeighborsOccupied

allPositions :: ChartState [(Int,Int)]
allPositions = do chrt <- get
                  let ymax = length chrt - 1
                      xmax = (length.head$chrt) -1
                  return [(x,y)|x<- [0..xmax], y<- [0..ymax]]

positionsToChange :: ChartState [(Position,Char)]
positionsToChange = do positions <- allPositions
                       toOccupy  <- mapM becomesOccupied positions
                       toEmpty   <- mapM becomesEmpty positions
                       let posToOccupy = map fst . filter ((==True).snd) $ zip positions toOccupy
                           posToEmpty  = map fst . filter ((==True).snd) $ zip positions toEmpty
                           toOccupiedList = zip posToOccupy (repeat '#')
                           toEmptyList    = zip posToEmpty (repeat 'L')
                       return $ toOccupiedList ++ toEmptyList

loop :: ChartState ()
loop = do pstsNchrs <- positionsToChange
          if length pstsNchrs > 0 then
            do changeCells pstsNchrs
               loop
          else
            return ()

changeCells ::  [(Position,Char)] -> ChartState ()
changeCells psNchs = do chrt <- get
                        let rowOrderedChanges = sortWith (snd.fst) psNchs
                            groupedPerRow = groupBy (\x y->(snd.fst$x)==(snd.fst$y)) rowOrderedChanges
                            formatted = map changeFormat groupedPerRow
                            rowChangesPairs = sortWith (fst) $ completeLst [0..(length chrt -1)] formatted
                            newChrt = zipWith changeRow rowChangesPairs chrt
                        put newChrt
    where changeFormat pps@(((x,y),chr):ps)= (y, map (\((x,y),ch)->(x,ch)) pps)
          completeLst ns pxs = foldr merge pxs $ zip ns $ repeat []
          merge (n,pxxs) lst = if n`elem`(map fst lst) then lst else (n,pxxs):lst

changeRow :: (Int,[(Int, Char)]) -> Row -> Row
changeRow (n, posChrs) rw  = foldr oneChange rw posChrs
    where oneChange (x,chr) row = (take x row) ++ (chr:(drop (x+1) row))

upperRow :: Position -> ChartState [Char]
upperRow (x,y) = do chrt <- get
                    if y>0 && x>0 && x < ((length.head$chrt)-1) then
                      return $ take 3.drop (x-1)$ chrt!!(y-1)
                    else
                      if y>0 && (x==0 || x== (length.head$chrt) -1) then
                        return $ take 2.drop (x-1)$ chrt!!(y-1)
                      else
                        return []

lowerRow :: Position -> ChartState [Char]
lowerRow (x,y) = do chrt <- get
                    if (y+1)<length chrt && x>0 && (x+1) < (length.head$chrt) then
                      return $ take 3.drop (x-1)$ chrt!!(y+1)
                    else
                      if y< (length chrt -1) && (x==0 || x==(length.head$chrt)-1) then
                        return $ take 2.drop (x-1)$ chrt!!(y+1)
                      else
                        return []

leftRightCells :: Position -> ChartState [Char]
leftRightCells (x,y) = do chrt <- get
                          if x>0 && x < (length.head$chrt)-1 then
                            return $ ((chrt!!y)!!(x-1)):[(chrt!!y)!!(x+1)]
                          else
                            if x==0 then
                              return $ [(chrt!!y)!!(x+1)]
                            else
                              return $ [(chrt!!y)!!(x-1)]
-}

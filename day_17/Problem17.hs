module Problem17 where
import Control.Monad.State.Lazy
import Parsing

{-==================== Part 2 =====================-}
type ConwayGame4d = State ConwayMap4d
type ConwayMap4d = [Position4d]
type Position4d = (Int,Int,Int,Int)

input2 :: IO ConwayMap4d
input2 = do actives <- input1
            return$(\(x,y,z)-> (x,y,z,0))<$>actives

answer2 :: IO Int
answer2 = do initState <- input2
             let finalState = execState (sequence.replicate 6$ step4d) initState
             return $ length finalState

step4d :: ConwayGame4d ()
step4d = do actives <- get
            if length actives == 0 then
              return ()
            else
              do let xMax = foldr1 max$(\(x,y,z,w)->x)<$>actives
                     yMax = foldr1 max$(\(x,y,z,w)->y)<$>actives
                     zMax = foldr1 max$(\(x,y,z,w)->z)<$>actives
                     wMax = foldr1 max$(\(x,y,z,w)->w)<$>actives
                     xMin = foldr1 min$(\(x,y,z,w)->x)<$>actives
                     yMin = foldr1 min$(\(x,y,z,w)->y)<$>actives
                     zMin = foldr1 min$(\(x,y,z,w)->z)<$>actives
                     wMin = foldr1 min$(\(x,y,z,w)->w)<$>actives
                     allPositions = [(x,y,z,w)| x<-[(xMin-1)..(xMax+1)], y<-[(yMin-1)..(yMax+1)],z<-[(zMin-1)..(zMax+1)],w<-[(wMin-1)..(wMax+1)]]
                     activeFrom4d pos = pos`isActiveBecauseOf4d`actives
                 put$ filter (activeFrom4d) allPositions

isActiveBecauseOf4d :: Position4d -> [Position4d] -> Bool
isActiveBecauseOf4d pos@(x,y,z,w) actives = (isActive&&twoOrThreeActiveNeighbors)||(not isActive && threeActiveNeighbors)
   where isActive                  = pos`elem`actives
         twoOrThreeActiveNeighbors = (length (filter (`elem`actives) neighbors))`elem`[2,3]
         threeActiveNeighbors      = 3 == length (filter (`elem`actives) neighbors)
         neighbors                 = filter (/=pos) [(x1,y1,z1,w1)|x1<-[x-1,x,x+1],y1<-[y-1,y,y+1],z1<-[z-1,z,z+1],w1<-[w-1,w,w+1]]

{-=================== Part 1 ======================-}
type ConwayGame = State ConwayMap
type ConwayMap = [Position]
type Position = (Int,Int,Int)

input1 :: IO ConwayMap
input1 = do str <- readFile "data17.txt"
            return.fst.head$parse conwayParser str

answer1 :: IO Int
answer1 = do initState <- input1
             let finalState = execState (sequence.replicate 6$ step) initState
             return $ length finalState
{-================================================-}
step :: ConwayGame ()
step = do actives <- get
          if length actives == 0 then
            return ()
          else
            do let xMax = foldr1 max$(\(x,y,z)->x)<$>actives
                   yMax = foldr1 max$(\(x,y,z)->y)<$>actives
                   zMax = foldr1 max$(\(x,y,z)->z)<$>actives
                   xMin = foldr1 min$(\(x,y,z)->x)<$>actives
                   yMin = foldr1 min$(\(x,y,z)->y)<$>actives
                   zMin = foldr1 min$(\(x,y,z)->z)<$>actives
                   allPositions = [(x,y,z)| x<-[(xMin-1)..(xMax+1)], y<-[(yMin-1)..(yMax+1)],z<-[(zMin-1)..(zMax+1)]]
                   activeFrom pos = pos`isActiveBecauseOf`actives
               put$ filter (activeFrom) allPositions

isActiveBecauseOf :: Position -> [Position] -> Bool
isActiveBecauseOf pos@(x,y,z) actives = (isActive&&twoOrThreeActiveNeighbors)||(not isActive && threeActiveNeighbors)
   where isActive                  = pos`elem`actives
         twoOrThreeActiveNeighbors = (length (filter (`elem`actives) neighbors))`elem`[2,3]
         threeActiveNeighbors      = 3 == length (filter (`elem`actives) neighbors)
         neighbors                 = filter (/=pos) [(x1,y1,z1)|x1<-[x-1,x,x+1],y1<-[y-1,y,y+1],z1<-[z-1,z,z+1]]

{-================= Parsing bit of code =================-}
alive :: Char -> Bool
alive = (=='#')

conwayParser :: Parser ConwayMap
conwayParser = do lns <- some.newLineToken.some $ sat (/='\n')
                  let xCoordsSets = xParser<$>lns
                      yMax        = length lns
                      tuples      = concat.zipWith zip xCoordsSets$repeat<$>[0..yMax]
                  return.map threeTuple.zip tuples$repeat 0
   where xParser :: String -> [Int]
         xParser str = map fst $ filter (alive.snd) $ zip [0..(length str -1)] str
         threeTuple :: ((a,a),a) -> (a,a,a)
         threeTuple ((x,y),z) = (x,y,z)
         newLineToken :: Parser a -> Parser a
         newLineToken p = do many $ char '\n'
                             result <- token p
                             many $ char '\n'
                             return result

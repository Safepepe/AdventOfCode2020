module Problem12 where
import Control.Monad.State.Lazy
import Parsing

{-===========  Part 2 bit of code ===========-}

{-===========  General bit of code ===========-}

type WayPoint = (Int,Int)
type FerryState2 = State (Position, WayPoint)

initState2 :: (Position, WayPoint)
initState2 = ((0,0),(10,1))

answer2 :: IO Int
answer2 = input >>= (\ins -> do let (endPos,waypt) = execState (loop2 ins) initState2
                                return $ manhattanDistance (0,0) endPos)

{-===========  technical bit of code ===========-}
--Each time we rotate, we do so by round angles.
rotateL90 :: Position -> Position
rotateL90 (x,y) = (-y,x)
rotateR90 :: Position -> Position
rotateR90 (x,y) = (y,-x)

interpreter2 :: Instruction -> FerryState2 ()
interpreter2 (cmd, n) = do ((x,y),(wx,wy)) <- get
                           case cmd of
                             MoveForward -> put $ ((x+n*wx,y+n*wy),(wx,wy))
                             LeftRotate -> put $ ((x,y), turnLeft (n`div`90) (wx,wy))
                             RightRotate -> put $ ((x,y),turnRight (n`div`90) (wx,wy))
                             Move North -> put $ ((x,y), (wx,wy+n))
                             Move East -> put $ ((x,y), (wx+n,wy))
                             Move South -> put $ ((x,y), (wx,wy-n))
                             Move West -> put $ ((x,y), (wx-n,wy))
    where turnLeft k waypt = foldr ($) waypt $replicate k rotateL90
          turnRight k waypt = foldr ($) waypt $replicate k rotateR90

loop2 :: [Instruction] -> FerryState2 ()
loop2 = mapM_ interpreter2


{-===========  Part 1 bit of code ===========-}

{-===========  General bit of code ===========-}
--Definitions
type Position = (Int,Int)
data Command = Move Direction | MoveForward | LeftRotate | RightRotate
  deriving(Eq, Show)
data Direction = North | South | East | West
  deriving(Eq, Show)
type FerryState = State (Position, Int)
type Instruction = (Command,Int)

input :: IO [Instruction]
input = do inpt <- readFile "data12.txt"
           return $ map (fst.head.parse instruction)$ lines inpt
--Answer

initState1 :: (Position,Int)
initState1 = ((0,0), 1)

answer1 :: IO Int
answer1 = input >>= (\ins -> do let (endPos,d) = execState (loop1 ins) initState1
                                return $ manhattanDistance (0,0) endPos)
{-===========  technical bit of code ===========-}
--Each time we rotate, we do so by round angles.


interpreter1 :: Instruction -> FerryState ()
interpreter1 (cmd, n) = do ((x,y),d) <- get
                           case cmd of
                             MoveForward -> put $ advanceBy n ((x,y),d)
                             LeftRotate -> put $ ((x,y), (d - n`div`90)`mod`4)
                             RightRotate -> put $ ((x,y), (d + n`div`90)`mod`4)
                             Move North -> put $ ((x,y+n), d)
                             Move East -> put $ ((x+n,y), d)
                             Move South -> put $ ((x,y-n), d)
                             Move West -> put $ ((x-n,y), d)
    where advanceBy k ((x1,y1),d1) = case directions!!d1 of
                                      North -> ((x1,y1+k),d1)
                                      East -> ((x1+k,y1),d1)
                                      South -> ((x1,y1-k),d1)
                                      West -> ((x1-k,y1),d1)
          directions = [North,East,South,West]

loop1 :: [Instruction] -> FerryState ()
loop1 = mapM_ interpreter1

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)


{-===========  Parsing bit of code ===========-}

instruction :: Parser Instruction
instruction = do lttr <- letter
                 n <- nat
                 case lttr of
                   'N' -> return (Move North,n)
                   'S' -> return (Move South,n)
                   'E' -> return (Move East,n)
                   'W' -> return (Move West,n)
                   'F' -> return (MoveForward,n)
                   'L' -> return (LeftRotate,n)
                   'R' -> return (RightRotate,n)

module Problem12 where
import Control.Monad.State.Lazy
import Data12 (inputIO, Instruction, Direction(North, South, East, West),
               Command(Move, MoveForward, LeftRotate, RightRotate))

{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type Position = (Int,Int)
type Angle = Int --Angle theta with the x axis. Think of polar coordinates!
type Ferry = (Position, Angle)

initState1 :: (Position, Angle)
initState1 = ((0,0), 0) --Lookwing eastward

interpreter1 :: Instruction -> Ferry -> Ferry
interpreter1 (cmd, n) ((x,y), theta) =
      case cmd of
        MoveForward -> advanceBy n ((x,y), theta`mod`360)
        LeftRotate  -> ((x,y), theta + n)
        RightRotate -> ((x,y), theta - n)
        Move North  -> ((x,y+n), theta)
        Move East   -> ((x+n,y), theta)
        Move South  -> ((x,y-n), theta)
        Move West   -> ((x-n,y), theta)
 where advanceBy k ((x1,y1), alpha) = case alpha of
                                        0 -> ((x1+k,y1), alpha)
                                        90 -> ((x1,y1+k), alpha)
                                        180 -> ((x1-k,y1), alpha)
                                        270 -> ((x1,y1 -k), alpha)

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
{-=----------------------------------=-}
answer1 :: IO Int
answer1 = do instructs <- inputIO
             let instructsR = reverse instructs --because we use foldr
                 endState   = foldr interpreter1 initState1 instructsR
             return$ manhattanDistance (fst initState1) (fst endState)
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
type Waypoint = Position --coordinates relative to the ship
type Ferry2  = (Position, Waypoint)

initState2 :: (Position, Waypoint)
initState2 = ((0,0),(10,1))

interpreter2 :: Instruction -> Ferry2 -> Ferry2
interpreter2 (cmd, n) ((x,y),(wx,wy)) =
                  case cmd of
                    MoveForward -> ((x+n*wx,y+n*wy),(wx,wy))
                    LeftRotate -> ((x,y), turnLeft (n`mod`360) (wx,wy))
                    RightRotate -> ((x,y), turnRight (n`mod`360) (wx,wy))
                    Move North -> ((x,y), (wx,wy+n))
                    Move East -> ((x,y), (wx+n,wy))
                    Move South -> ((x,y), (wx,wy-n))
                    Move West -> ((x,y), (wx-n,wy))
  where
    turnRight k (p,q) = turnLeft (360 - k) (p,q)
    turnLeft k (p,q) = case k of
                           0   -> (p,q)
                           90  -> (-q,p)
                           180 -> (-p,-q)
                           270 -> (q, -p)
                           360 -> (p,q)
{-=----------------------------------=-}
answer2 :: IO Int
answer2 = do instructs <- inputIO
             let instructsR = reverse instructs --because we use foldr
                 endState   = foldr interpreter2 initState2 instructsR
             return$ manhattanDistance (fst initState2) (fst endState)

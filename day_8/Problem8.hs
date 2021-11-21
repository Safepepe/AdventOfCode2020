module Problem8 where
import Data8 (inputIO, Command(Acc,Jmp,Nop), Tape)
import Control.Monad.State.Lazy

type Accumulator = Int
type Position = Int
type PastPositions = [Int]
{-data Command = Acc Int | Jmp Int | Nop Int
   deriving(Show)
type Tape = [Command] -}
type GameLoop = State (Accumulator, PastPositions)
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
initState = (0,[0]) -- positions start at 0

getPast :: GameLoop PastPositions
getPast = get >>= return . snd
getAcc :: GameLoop Accumulator
getAcc = get >>= return . fst
putAcc :: Accumulator -> GameLoop ()
putAcc a = do ps <- getPast
              put (a,ps)
move :: Int -> GameLoop ()
move n = do (ac,ps) <- get
            if null ps then
              put (ac, [n])
            else
              put (ac, (n + head ps) : ps)

getCurrentPos :: GameLoop Position
getCurrentPos = do ps <- getPast
                   if null ps then
                     put (0, [0]) >> return 0
                   else
                     return $ head ps

stepThrough :: Tape -> GameLoop Tape
stepThrough tape = do current <- getCurrentPos
                      if current >= length tape then -- stop when tape is over
                        return tape
                      else
                        do let cmd = tape !! current
                           ps <- getPast
                           nSpaces <- execCmd cmd
                           move nSpaces
                           if (nSpaces + current) `elem` ps then --we avoid infinite loops
                              return tape
                           else
                              stepThrough tape

execCmd :: Command -> GameLoop Int
execCmd (Nop _) = return 1 --advance by 1
execCmd (Jmp n) = return n
execCmd (Acc n) = do a <- getAcc
                     putAcc $ a+n
                     return 1 --advance by 1

runGameOn :: Tape -> (Accumulator, PastPositions)
runGameOn tape = execState (stepThrough tape) initState

{-=-----------------------------------=-}
answer1 :: IO Int
answer1 = inputIO >>= return.fst.runGameOn
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
swapNopJmp :: Command -> Command
swapNopJmp (Acc n) = Acc n
swapNopJmp (Nop n) = Jmp n
swapNopJmp (Jmp n) = Nop n

change :: Position -> Tape -> Tape
change p tape = take p tape ++ [swapNopJmp (tape !!  p)] ++ drop (p+1) tape

isCorrect :: Tape -> Bool
isCorrect tape = (>= length tape) . head . snd . runGameOn $ tape

corrected :: Tape -> Tape  --Brute forcing it.
corrected tape = head correctTapes
  where pastPos       = snd $ runGameOn tape
        changedTapes  = map (`change` tape) pastPos
        correctTapes  = filter isCorrect changedTapes
        
{-=-----------------------------------=-}
answer2 :: IO Accumulator
answer2 = inputIO >>= return.fst.runGameOn.corrected

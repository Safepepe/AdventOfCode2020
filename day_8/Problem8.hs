module Problem8 where
import Parsing
import Control.Monad.State.Lazy

type Accumulator = Int
type Position = Int
type PastPositions = [Int]
data Command = Acc Int | Jmp Int | Nop Int
   deriving(Show)
type Tape = [Command]
type GameLoop = State (Accumulator, PastPositions)
{-============================================================================-}
{-=============================     Actual Code   ============================-}
{-============================================================================-}
initState = (0,[0]) --the list must contain an element

pastPositions :: GameLoop PastPositions
pastPositions = get >>= return . snd
getAccumulator :: GameLoop Accumulator
getAccumulator = get >>= return . fst
putAccumulator :: Accumulator -> GameLoop ()
putAccumulator a = do ps <- pastPositions
                      put (a,ps)
move :: Int -> GameLoop ()
move n = do (ac,ps) <-get
            put $ (ac, (n + head ps) : ps)

stepThrough :: Tape -> GameLoop Tape
stepThrough tape = do ps <- pastPositions
                      let current = head ps
                      if (current +1) > length tape then -- stop when tape is over
                        return tape
                      else
                        do let cmd = tape !! current
                           n <- execCmd cmd
                           move n
                           if (n+current) `elem` ps then --we avoid infinite loops
                              return tape
                           else
                              stepThrough tape

execCmd :: Command -> GameLoop Int
execCmd (Nop _) = return 1
execCmd (Jmp n) = return n
execCmd (Acc n) = do a <- getAccumulator
                     putAccumulator $ a+n
                     return 1

runGameOn :: Tape -> (Accumulator, PastPositions)
runGameOn tape = execState (stepThrough tape) initState

{-answer to the first part-}

answer :: IO Int
answer = do input <- readFile "data8.txt"
            let tape    = makeTape input
                (a, ps) = runGameOn tape
            return a

{-answer to the second part-}

swapNopJmp :: Command -> Command
swapNopJmp (Acc n) = Acc n
swapNopJmp (Nop n) = Jmp n
swapNopJmp (Jmp n) = Nop n

change :: Position -> Tape -> Tape
change p tape = take (p) tape ++ [swapNopJmp (tape !!  p)] ++ drop (p+1) tape

isCorrect :: Tape -> Bool
isCorrect tape = (>= length tape) . head . snd . runGameOn $ tape

--Lazyness makes this function not suck so much (I think)
corrected :: Tape -> Tape
corrected tape = head correctTapes
  where pastPos       = snd $ runGameOn tape
        changedTapes  = map (`change` tape) pastPos
        correctTapes  = filter isCorrect changedTapes

answer2 :: IO Accumulator
answer2 = do input <- readFile "data8.txt"
             let tape    = makeTape input
                 (a, ps) = runGameOn $ corrected tape
             return a
{-============================================================================-}
{-=============================Parsing bit of code============================-}
{-============================================================================-}

acc,jmp,nop :: Parser String
acc = string "acc"
jmp = string "jmp"
nop = string "nop"

signedNum :: Parser Int
signedNum = (char '+' >> nat) <|> int

readAcc,readJmp,readNop :: Parser Command
readAcc  = do cmd <- token acc
              number <- signedNum
              return $ Acc number
readJmp = do cmd <- token jmp
             number <- signedNum
             return $ Jmp number
readNop = do cmd <- token nop
             number <- signedNum
             return $ Nop number

readCommand :: Parser Command
readCommand = readAcc <|> readJmp <|> readNop

makeTape :: String -> Tape
makeTape input = concat $ map toCommand inputLines
    where inputLines = lines input
          toCommand  = map fst . parse readCommand

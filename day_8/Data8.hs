module Data8 where
import Parsing

type Accumulator = Int
type Position = Int
type PastPositions = [Int]
data Command = Acc Int | Jmp Int | Nop Int
   deriving(Show)
type Tape = [Command]

filename = "data8.txt"

inputIO :: IO Tape
inputIO = readFile filename >>= return.tape

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

tape :: String -> Tape
tape input = concat $ map toCommand inputLines
    where inputLines = lines input
          toCommand  = map fst . parse readCommand

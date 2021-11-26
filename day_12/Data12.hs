module Data12 where
import Parsing

filename = "data12.txt"

inputIO :: IO [Instruction]
inputIO = readFile filename >>= return.fst.head.parse (many instructionP)
{------------------------------------------------------------------------------}
data Command = Move Direction | MoveForward | LeftRotate | RightRotate
  deriving(Eq, Show)
data Direction = North | South | East | West
  deriving(Eq, Show)
type Instruction = (Command,Int)

instructionP :: Parser Instruction
instructionP = do lttr <- letter
                  n <- nat
                  space
                  case lttr of
                    'N' -> return (Move North,n)
                    'S' -> return (Move South,n)
                    'E' -> return (Move East,n)
                    'W' -> return (Move West,n)
                    'F' -> return (MoveForward,n)
                    'L' -> return (LeftRotate,n)
                    'R' -> return (RightRotate,n)

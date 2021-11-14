module Data2 where
import Parsing

type Entry = (Int, Int, Char, String)
type Input = [Entry]

filename = "data2.txt"

entryP :: Parser Entry
entryP = do
  n1 <- token natural
  token $ char '-'
  n2 <- token natural
  c  <- token letter
  token $ char ':'
  password <- many letter
  space
  return (n1,n2,c,password)

inputP :: Parser Input
inputP = some entryP

inputIO :: IO Input
inputIO = readFile filename >>= return.fst.head.parse inputP

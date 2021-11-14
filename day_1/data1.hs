module Data1 where
import Parsing

--Here we parse the input. It will be a of type [Int]

type Input = [Int]

filename = "data1.txt"

inputParser :: Parser Input
inputParser = some $ natural <* space

inputIO :: IO Input
inputIO = readFile filename >>= return.fst.head.(parse inputParser)

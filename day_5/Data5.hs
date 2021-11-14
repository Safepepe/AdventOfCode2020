module Data5 where

filename = "data5.txt"
type BoardingPass = String

inputIO :: IO [BoardingPass]
inputIO = readFile filename >>= return.lines

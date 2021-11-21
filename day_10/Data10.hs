module Data10 where

filename = "data10.txt"

inputIO :: IO [Int]
inputIO = readFile filename >>= return.map read.lines

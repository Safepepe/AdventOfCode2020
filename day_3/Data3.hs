module Data3 where

type Map = [String]

filename = "data3.txt"

inputIO :: IO Map
inputIO = readFile filename >>= return.lines

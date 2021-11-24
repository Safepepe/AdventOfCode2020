module Data11 where

type Row = String
type Chart = [Row]

inputIO :: IO Chart
inputIO =  readFile "data11.txt" >>= return . lines

module Problem6 where
import Control.Monad.State.Lazy

type Input = String
type RawGroup = String

input :: IO String
input = readFile "data6.txt"

answer1:: IO ()
answer1 = input >>= (print . sum . map anyYesCount . toRawGroups)

answer2:: IO ()
answer2 = input >>= (print . sum . map everyYesCount . toRawGroups)

takeWhile2 :: (Char -> Bool) -> String -> String
takeWhile2 _ []  = []
takeWhile2 _ [x] = [x]
takeWhile2 cond (x:y:str)
        | not (cond x || cond y) = []
        | otherwise        = x:(takeWhile2 cond (y:str))

dropWhile2 :: (Char -> Bool) -> String -> String
dropWhile2 _ [] = []
dropWhile2 _ [x] = []
dropWhile2 cond (x:y:str)
        | not (cond x || cond y) = x:y:str
        | otherwise              = dropWhile2 cond (y:str)


getGroups :: Input -> State [RawGroup] ()
getGroups input = if length input > 0 then
                   do
                    stack <- get
                    let newVal = takeWhile2 (/='\n') input
                        rest = dropWhile (=='\n') $ dropWhile2 (/='\n') input
                    put $ newVal:stack
                    getGroups rest
                  else return ()

toRawGroups :: Input -> [RawGroup]
toRawGroups str = execState (getGroups str) []


anyYesCount :: RawGroup -> Int  --Not optimal at all !! But idc about that right now
anyYesCount str = length $ filter (`elem` str) alphabet
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

everyYesCount :: RawGroup -> Int
everyYesCount rawgrp = foldr allCount 0 alphabet
 where alphabet = "abcdefghijklmnopqrstuvwxyz"
       groupPeople = lines rawgrp
       allSaidYesTo chr = and $ map (elem chr) groupPeople
       allCount chr count = if allSaidYesTo chr then count+1 else count

module Problem2 where

isNumber :: Char -> Bool
isNumber c  = c `elem` "1234567890"

parseData :: String -> (Int,Int, Char, String)
parseData str = (minNum, maxNum, letter, password)
         where parts = words str --this is a list of 3 elements
               minNum = read . takeWhile isNumber $ parts!!0 :: Int
               maxNum = read . tail . dropWhile isNumber $ parts!!0 :: Int
               letter = head $ parts!!1
               password = parts!!2

correctPassword1 :: (Int, Int, Char, String) -> Bool
correctPassword1 (n1,n2,c,password) = n1 <= n && n <= n2
  where n = length $ filter (==c) password

correctPassword2 :: (Int,Int, Char, String) -> Bool
correctPassword2 (n1,n2,c,password) = (notTooSmall && fstMatches)
                                   || (bigEnough && sndMatches)
    where notTooSmall = length password >= n1
          bigEnough   = length password >= n2
          fstMatches   = password!!(n1-1) == c && password!!(n2-1) /= c
          sndMatches   = password!!(n1-1) /= c && password!!(n2-1) == c


howMany :: ((Int, Int, Char, String) -> Bool) -> [String] -> Int
howMany cond strs = length $ filter (cond.parseData) strs

answer2 = do
  str <- readFile "data2.txt"
  let dataBase = lines str
  print $ howMany correctPassword2 dataBase

answer1 = do
  str <- readFile "data2.txt"
  let dataBase = lines str
  print $ howMany correctPassword1 dataBase

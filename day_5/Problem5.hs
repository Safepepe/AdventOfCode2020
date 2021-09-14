module Problem5 where
import Data.List (sort)

{-Parameters-}
totalRows = 128  --0 through 128
lastRow = 127
totalColumns = 8 --0 through 7
lastColumn = 7
rowCodeLength = 7

input :: IO String
input = readFile "data5.txt"

answer1 :: IO ()
answer1 = input >>= (print . getMaxID)
  where getMaxID = foldr max 0 . map (toID . toSeat) .lines

answer2 :: IO ()
answer2 = input >>= ( print . deduceMyID . sortIDs )
  where sortIDs = sort . map (toID . toSeat) . lines
        deduceMyID xs = snd $ foldr func (last xs, last xs) xs
        func x (current, iD)  = if current - x >= 2 then (x, current -1)  else (x, iD)

type Row = Int
type Column = Int
type Seat = (Row, Column)
type ID = Int
type BoardingPass = String

--I assume that I work on the first part on the ticket string
toRow :: String -> Int
toRow = fst . foldl decode (0, lastRow)
  where decode (r1, r2) ch = if ch == 'F'
                             then  (r1 , r2 - div (r2-r1) 2 -1)
                             else  (r1 + 1 + div (r2-r1) 2, r2)
toColumn :: String -> Int
toColumn = fst . foldl decode (0,lastColumn)
  where decode (c1, c2) ch = if ch == 'L'
                             then  (c1 , c2 - div (c2-c1) 2 -1)
                             else  (c1 + 1 + div (c2-c1) 2, c2)

toSeat :: BoardingPass -> Seat
toSeat str = (toRow first7, toColumn last3)
      where (first7, last3) = splitAt rowCodeLength str

toID :: Seat ->  ID
toID (row, col) = row*8 + col

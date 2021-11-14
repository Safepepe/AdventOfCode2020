module Problem5 where
import Data5 (inputIO)
import Data.List (sort)

{-Parameters-}
totalRows = 128  --0 through 128
lastRow = 127
totalColumns = 8 --0 through 7
lastColumn = 7
rowCodeLength = 7

{-Part One-}
type Row = Int
type Column = Int
type Seat = (Row, Column)
type ID = Int
type BoardingPass = String

-------------------------------'Unsafe' functions-------------------------------
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
--------------------------------------------------------------------------------
toSeat :: BoardingPass -> Seat
toSeat str = (toRow first7, toColumn last3)
      where (first7, last3) = splitAt rowCodeLength str

toID :: Seat ->  ID
toID (row, col) = row*8 + col

answer1 :: IO Int
answer1 = do
  bPasses <- inputIO
  return$ getMaxID bPasses
   where getMaxID = foldr max 0 . map (toID . toSeat)


{-Part Two-}

sortedIDs :: [BoardingPass] -> [ID]
sortedIDs = sort . map (toID . toSeat)

deduceMyID :: [ID] -> ID
deduceMyID ids = snd $ foldr func (lastID, lastID) ids
  where
    lastID = last ids
    func x (current, iD)  = if current - x >= 2 then (x, current -1)  else (x, iD)

answer2 :: IO Int
answer2 = do
  bPasses <- inputIO
  return$ deduceMyID $ sortedIDs bPasses

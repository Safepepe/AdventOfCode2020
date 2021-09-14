module Problem16 where
import Parsing
import GHC.Exts (sortWith)

type FieldName = String
type Range = (Int,Int)
type Field = (FieldName,[Range])
type Ticket = [Entry]
type ErrorRate = Int
type Entry = Int
type Column = [Entry]
type Position = Int
type TicketPosition = Int
{-====================       Part 1        =======================-}
input1P :: IO ([Field],Ticket,[Ticket])
input1P = do str <- readFile "data16.txt"
             return $fst.head$parse inputParserP str

errorRate :: Ticket -> [Field] -> ErrorRate
errorRate tktEntries fcnds = sum $ filter (not.valid) tktEntries
  where  ranges = concat $ map snd fcnds
         inRange n (v1,v2) = n >= v1 && n <= v2
         valid n = or $ (inRange n)<$>ranges

answer1P :: IO ErrorRate
answer1P = do (fldConds, _,  tkts) <- input1P
              return $ sum $(`errorRate`fldConds)<$>tkts

{-====================       Part 2       =======================-}
input2P ::  IO ([Field],Ticket,[Ticket])
input2P = do (flds, myTkt, tkts) <- input1P
             let valid tkt = and $ (or)<$>(<$>flds)<$>((flip validOnOne)<$>tkt)
             return (flds, myTkt, filter valid tkts)

fieldsPerColumn :: IO [[FieldName]]
fieldsPerColumn = do (flds, _, tkts) <- input2P
                     let entryColumns = transpose tkts
                         fldsPerColumn    = (`possibleFields`flds)<$>entryColumns
                     return fldsPerColumn

whichFailOn :: [Ticket] -> Field -> Position -> [TicketPosition]
whichFailOn tkts fld n = fst<$>(filter (not.(fld`validOnOne`).snd) column)
   where column = zip [1..(length tkts)] $ map (!!(n-1)) tkts

solution2P :: IO [(Int,FieldName)]
solution2P = do fldsPerColumn <- fieldsPerColumn
                let indxdfldsPerColumns = zip [1..(length fldsPerColumn)] fldsPerColumn
                    srtdIndxdfldsPerColumns = sortWith (length.snd) indxdfldsPerColumns
                    sol                = paths [] srtdIndxdfldsPerColumns
                case sol of
                   Just ls -> return $sortWith (fst) $ head ls
                   Nothing -> return []

answer2P :: IO Int
answer2P = do sol <- solution2P
              (_,myTkt,_) <- input1P
              let newList   = filter ((=="departure").(take 9 ).snd) sol
                  positions = fst<$>newList
                  numbers   = ((myTkt!!).(\n->n-1))<$>(positions)
              return $ product numbers
{-================= Inefficient solution =============================-}
paths :: Eq a => [(Int,a)] -> [(Int,[a])] -> Maybe [[(Int,a)]]
paths pth []           = Just [pth]
paths pth ((n,[]):lss) = Nothing
paths pth ((n,(l:ls)):lss)
  | l `elem`(snd<$>pth) = paths pth ((n,ls):lss)
  | otherwise           = paths pth ((n,ls):lss) <> paths ((n,l):pth) lss
{-================== tool functions =====================-}
transpose :: [[a]]-> [[a]]  --OK
transpose []              = []
transpose ([]:xs)         = []
transpose ys@((x:xs):xss) = (head<$>ys) : transpose (tail<$>ys)

validOnOne:: Field -> Entry -> Bool  --OK
validOnOne (str, rgs) fldEntry = or $ (fldEntry`inRange`)<$>rgs
 where inRange n (v1,v2) = (n>=v1)&&(n<=v2)

validOnAll :: Field -> Column -> Bool --OK
validOnAll fld column = and $ (fld`validOnOne`)<$>column

possibleFields :: Column -> [Field] -> [FieldName] --OK
possibleFields column flds = (fst<$>).filter (`validOnAll`column)$ flds


{-====================    Parsing bit of code     =======================-}
inputParserP :: Parser ([Field],Ticket,[Ticket])
inputParserP = do fieldConds <- some fieldP
                  mytkt  <- myTicketP
                  nearbyTkts <- nearbyTicketsP
                  return (fieldConds, mytkt, nearbyTkts)

fieldP :: Parser Field
fieldP = do fld <- some $ sat (/=':')
            char ':'
            rs <- some rangeP
            space
            many $ char '\n'
            return (fld, rs)

rangeP :: Parser Range
rangeP = do space
            many $ string "or"
            r1 <- nat
            char '-'
            r2 <- nat
            space
            many $ string "or"
            return (r1,r2)

myTicketP :: Parser Ticket
myTicketP = do string "your ticket:"
               space
               many$ char '\n'
               tckt <- ticketP
               space
               many $ char '\n'
               return tckt

nearbyTicketsP :: Parser [Ticket]
nearbyTicketsP = do string "nearby tickets:"
                    space
                    many $ char '\n'
                    tckts <- many ticketP
                    return tckts
ticketP :: Parser Ticket
ticketP = do lst <- some $ nat >>= (\n -> many (char ',') >> return n)
             space
             many $ char '\n'
             return lst

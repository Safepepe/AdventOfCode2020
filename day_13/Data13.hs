module Data13 where
import Parsing

filename = "data13.txt"

inputIO :: IO (TimeStamp,[(BusID,WaitTime)])
inputIO = do str <- readFile filename
             let lns = lines str --only 2 lines
                 timeStamp0 = read (lns!!0) :: Int
                 ln2     = lns!!1
                 busList0 = fst.head$parse busIDsParserP ln2
                 busList = filter ((/= -1).fst) $ zip busList0 [0..(length busList0 -1)]
             return $ (timeStamp0, map (\(x,y)-> (x,-y)) busList)

type WaitTime = Int
type TimeStamp = Int
type BusID = Int

busIDsParserP :: Parser [BusID]
busIDsParserP = some $ busID
  where busID = do many $ char ','
                   n <- nat <|> xParser
                   many $ char ','
                   return n
        xParser = char 'x' >> return (-1)

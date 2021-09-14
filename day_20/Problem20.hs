module Problem20 where
import Parsing
import qualified Data.IntMap.Lazy as M

type Image = [String]
type Tile = (Int, Image)
type ImageArray = M.IntMap Image



input1 :: IO ImageArray
input1 = readFile "data20.txt" >>= return.fst.head.parse inputP

answer1 :: IO ()
answer1 = do imgArray <- input1
             print $ M.lookup 1217 imgArray --to test if the code worked.


{-====================Parsing bit of code ====================================-}
tileP :: Parser Tile
tileP = do string "Tile"*> space
           n <- nat
           token $ char ':'
           image <- some $ token $ some $ char '#' <|> char '.'
           return (n,image)

inputP :: Parser ImageArray
inputP = do tiles <- some tileP
            return $ M.fromList tiles

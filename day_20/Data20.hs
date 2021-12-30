module Data20 where
input Parsing

type Image = [String]
type Tile = (Int, Image)

inputIO :: IO [Tile]
inputIO = readFile "data20.txt" >>= return.fst.head.parse (some tileP)

filename = "data20.txt"

tileP :: Parser Tile
tileP = do token $ string "Tile"
           n <- nat <* string ":\n"
           image <- some $ lineP <* (many $ char '\n')
           return (n,image)
  where
    lineP = some $ char '#' <|> char '.'

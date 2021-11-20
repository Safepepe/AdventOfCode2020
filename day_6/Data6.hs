module Data6 where
import Parsing

type Person = String
type Group = [Person]

filename = "data6.txt"

personP:: Parser Person
personP = some letter <* string "\n"

groupP :: Parser Group
groupP = some personP

groupsP :: Parser [Group]
groupsP = some $ (groupP <* char '\n') <|> groupP

inputIO :: IO [Group]
inputIO = readFile filename >>= return.fst.head.(parse groupsP)

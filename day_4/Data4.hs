module Data4 where
import Parsing

{-Entries can contain some of the following data
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID) -}
type Key = String
type Value = String
type Field = (Key,Value)
type Entry = [Field]

filename = "data4.txt"
fieldNames = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

field :: String -> Parser Field
field str = do
  key <- string str
  char ':'
  val <- some $ sat (not. (`elem` [' ', '\n']))
  return (key,val)

fieldParsers :: [Parser Field]
fieldParsers = map ((<* spaceP).field) fieldNames
   where spaceP = (string "\n" <|> some (char ' '))

entryP :: Parser Entry
entryP = string "\n" *> some fields <|> some fields
   where fields = foldr1 (<|>) fieldParsers

inputIO :: IO [Entry]
inputIO = readFile filename >>= return.fst.head.(parse$some entryP)

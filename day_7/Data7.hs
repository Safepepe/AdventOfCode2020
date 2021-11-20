module Data7 where
import Parsing

{-I believe there is probably a problem with punctuation and/or newline characters.-}
filename = "/Users/pjaramil/Documents/gitFiles/AdventOfCode2020/day_7/data7.txt"

inputIO :: IO [Rule]
inputIO = readFile filename >>= return.map fst.concat.(parse ruleP <$>).lines


type Container = String
type Content = (Int, Container)
type Rule = (Container, [Content])

bags = ["bag", "bags"]
punctuationList = ":;,.!?"

punctuation :: Parser ()
punctuation = do
  sat (`elem` punctuationList)
  return ()

word :: Parser String
word =  do
  many $ token punctuation
  wrd <- some letter
  many $ token punctuation
  return wrd

stopWhenWord :: (String -> Bool) -> Parser [String]
stopWhenWord cond = do
  fstWrd <- token word
  if cond fstWrd then
    return [] --this gets rid of the word
  else
    (fstWrd:) <$> stopWhenWord cond

getContainer :: Parser Container
getContainer = do
  containerName <- stopWhenWord (`elem` bags) --gives the name of the bag
  many $ token $ string "contains" <|> string "contain"
  return $ unwords containerName

content :: Parser Content
content =  do
  qty <- nat
  bag <- getContainer
  return (qty,bag)

ruleP :: Parser Rule
ruleP = do
  bag <- getContainer
  contents <- many content
  throwAwayRest
  return (bag, contents)
 where throwAwayRest = many $ sat (\x -> True)

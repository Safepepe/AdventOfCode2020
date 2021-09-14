module Problem19 where
import Parsing
import Control.Monad.Reader
import Data.Either
import qualified Data.IntMap.Lazy as M

type Rules = M.IntMap Rule
type Rule  = Either Char [Chain]
type Chain = [Int]
type RuleBook = Reader Rules

{-============================+ Part 1 +======================================-}
input1 :: IO (Rules,[String])
input1 = readFile "data19.txt" >>= return.fst.head.parse inputParser

answer1 :: IO ()
answer1 = do (ruleBook, dataStrs) <- input1
             let rule0 = ruleBook M.! 0
                 rule0Parser = runReader (matchRule True rule0) ruleBook
                 successList = filter (not.null.parse rule0Parser) dataStrs
             putStr "success lines: " >> (print.length) successList
             putStr "total lines: "  >> (print.length) dataStrs

{-===============================+ Part 2 +===================================-}
newRule8  = Right [[42],[42,8]]
newRule11 = Right [[42,31],[42,11,31]]

input2 :: IO (Rules,[String])
input2 = readFile "data19.txt" >>= parseInput
  where parseInput = return.fst.head.parse inputParser

answer2 :: IO ()
answer2 = do (ruleBook,dataStrs) <-input2
             let newRuleBook = foldr ($) ruleBook $ [M.insert 8 newRule8, M.insert 11 newRule11]
                 rule0 = newRuleBook M.! 0
                 rule0Parser = runReader (matchRule True rule0) newRuleBook
                 successList = filter (not.null.parse rule0Parser) dataStrs
             putStr "success lines: " >> (print.length) successList
             putStr "total lines: "  >> (print.length) dataStrs

{-=========== tool functions ===========-}

matchRule :: Bool -> Rule ->  RuleBook (Parser ()) --matching implies consuming the whole string.
matchRule True  (Left c)         = return$ char c *> endOfStr
matchRule False (Left c)         = return$ char c *> pure ()
matchRule _     (Right [])       = return$ empty
matchRule finish  (Right (ch:chs)) = --The list only has 2 elements in the problem. 
        do listOfRules <- sequence$lookUpRule<$>ch
           starterParsers  <- sequence$(matchRule False)<$>(init listOfRules) --all but last rule in a chain.
           endParser <- matchRule finish $ last listOfRules --last rule in a chain
           let firstChain = foldr (*>) (endParser) starterParsers
           secondChain <- matchRule finish (Right chs)
           return $ firstChain <> secondChain --order agnostic. unlike <|>. --This makes it easier

lookUpRule :: Int -> RuleBook Rule
lookUpRule = reader.(flip (M.!))

{-==================Parsing bit of code ==============================-}

inputParser :: Parser (Rules,[String])
inputParser = do ruleBook <- rulesParser
                 strs <- dataParser
                 return (ruleBook, strs)

dataParser :: Parser [String]
dataParser = some $ dataLineParser
  where dataLineParser = do pLine <- some letter
                            many $char '\n'
                            return pLine

rulesParser :: Parser Rules
rulesParser = do ruleBook <- some ruleParser
                 many$ char '\n'
                 return$ M.fromList ruleBook

ruleParser :: Parser (Int, Rule)
ruleParser = do n <- nat
                token $ char ':'
                ruleValue  <- letterParser <|>  doubleChainParser <|> chainParser
                many$char '\n'
                return (n,ruleValue)

letterParser :: Parser Rule
letterParser = do char '\"'
                  x <- letter
                  char '\"'
                  token $ many $ char '\n'
                  return $ Left x

chainParser :: Parser Rule
chainParser = do chain1 <- some$ spaces *> nat <* spaces
                 token.many$ char '\n'
                 return$ Right $ [chain1]
  where spaces = many$sat (==' ')

doubleChainParser :: Parser Rule
doubleChainParser = do chain1 <- chainParser
                       token$char '|'
                       chain2 <- chainParser
                       return$ (++)<$>chain1<*>chain2

module Problem18 where
import Parsing

{-========== Part 2 =========-}
expreP2 :: Parser Int
expreP2 = productP2 <|> sumP2 <|> subExpreP2 <|> valueP2

valueP2 :: Parser Int
valueP2  = token nat

sumP2 :: Parser Int
sumP2 = do expr1 <- token $ subExpreP2 <|> valueP2
           char '+'
           expr2 <- token $ sumP2 <|> subExpreP2 <|>valueP2
           return $ expr1 + expr2

productP2 :: Parser Int
productP2 = do expr1 <- token $ sumP2 <|> subExpreP2 <|> valueP2
               char '*'
               expr2 <- token $ expreP2
               return $ expr1 * expr2

subExpreP2 :: Parser Int
subExpreP2 = do token $ char '('
                expr1 <- token $ expreP2
                token $ char ')'
                return expr1

answer2 :: IO Int
answer2 = do str <- readFile "data18.txt"
             let strLines      = lines str
                 {-failedResults  = filter (null.parse expreP2) strLines
                 partialFailedResults = filter (not.null.snd).(head<$>).filter (not.null) $ (parse expreP2)<$>strLines
                 example = "(9 + 3 + (3 + 7 + 3 + 8 + 3 + 6) + (7 + 2 + 4 + 4 * 5 * 2) + 6 + 6) * 2"
             putStrLn "the number of partial fails is"
             print$ length partialFailedResults
             putStrLn "the number of failed  results is "
             print$ length failedResults-}
             return$ sum$ fst.head.(parse expreP2)<$>strLines


{-========== Part 1 ========-}

expreP :: Parser Int
expreP = productP <|> sumP <|> subExpreP <|> valueP

valueP :: Parser Int
valueP = token nat

sumP :: Parser Int
sumP = do expr1 <- token $ valueP <|> subExpreP
          token $ char '+'
          expr2 <- token $ expreP
          return $ expr1 + expr2

productP :: Parser Int
productP = do expr1 <- token $ valueP <|> subExpreP
              token $ char '*'
              expr2 <- token $ expreP
              return $ expr1 * expr2

subExpreP :: Parser Int
subExpreP = do token $ char '('
               expr1 <- token$ expreP
               token $ char ')'
               return expr1

answer1 :: IO Int
answer1 = do str <- readFile "data18.txt"
             let strLines = correct.reverse<$>lines str
                 parseResults  = head.(parse expreP)<$>strLines
                 results = fst<$>parseResults
             print$results
             return.sum$results

{-==========================   Tool functions  ===============================-}


correct :: String -> String
correct [] = []
correct (c:cs)
   | c == '('  = ')':correct cs
   | c == ')'  = '(':correct cs
   | otherwise =  c :correct cs

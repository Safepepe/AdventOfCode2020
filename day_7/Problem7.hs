module Problem7 where
import Parsing
import Control.Monad.State.Lazy

type Container = String
type Content = (Int, Container)
type Rule = (Container, [Content])


{-============================================================================-}
----------------------------This is the parsing part of the code
{-============================================================================-}
bags = ["bag", "bags","bags,", "bag,", "bags.", "bag."]
punctuation = ":;,.!?"

symbols :: Parser ()
symbols = do many (sat (`elem` punctuation))
             return ()

word :: Parser String
word =  token $ many $ sat (/=' ')


stopWhenWord :: (String -> Bool) -> Parser [String]
stopWhenWord cond = do space --get rid of spaces
                       fstWrd <- word
                       if cond fstWrd then
                         return [] --this gets rid of the word
                       else
                         (fstWrd:) <$> stopWhenWord cond

getContainer :: Parser Container
getContainer = do containerWrds <- stopWhenWord (`elem` bags)
                  space
                  many $ string "contains" <|> string "contain"
                  space
                  return $ unwords containerWrds

content :: Parser Content
content =  do n <- nat
              bag <- getContainer
              symbols --gets rid of commas essentially
              space --gets rid of spaces
              return (n,bag)

rule :: Parser Rule
rule = do bag <- getContainer
          space
          contents <- many content
          return (bag, contents)
{-============================================================================-}
----------------------------This is where we actually work at the rule level
{-============================================================================-}


makeRules :: String -> [Rule]
makeRules  = map fst . concat . map (parse rule) . lines

nextColors :: Container -> [Rule] -> [Container]
nextColors cont rules  = map snd itsContents
     where itsRules    = filter ((==cont).fst) rules
           itsContents = concat . map snd $ itsRules

allColorsState :: Container -> [Rule] -> State [Container] ()
allColorsState cont rules = let nextCs = nextColors cont rules in
                       if length nextCs == 0 then
                         return ()
                       else
                        do currentColors <- get
                           let newColors = makeSet $ nextCs ++ currentColors
                           put newColors
                           mapM_ (`allColorsState` rules) nextCs

allColors :: Container -> [Rule] -> [Container]
allColors cont rules = execState (allColorsState cont rules) []

expand :: Content -> [Rule] -> [Content]
expand (n,container) rules = map (timesFst n) contentOfOne
  where itsRuleSet = filter ((==container).fst) rules
        contentOfOne = snd . head $ itsRuleSet
        timesFst k (p,q) = (p*k,q)

totalContentCount :: Content -> [Rule] -> Int
totalContentCount contnt@(n,name) rules =
                 let fstCntnts = expand contnt rules in
                 sum (map fst fstCntnts) + (sum $ map (`totalContentCount` rules) fstCntnts)


{-============================================================================-}
makeSetState  :: Eq a => [a] ->  State [a] ()
makeSetState []     = return ()
makeSetState (c:cs) = do cleanList <- get
                         if (c `elem` cleanList) then
                           do makeSetState cs
                         else
                           do put $ c:cleanList
                              makeSetState cs
makeSet :: Eq a => [a] -> [a]
makeSet xs =  snd $ runState (makeSetState xs) []
{-============================================================================-}
problemBag = "shiny gold"

answer2 :: IO ()
answer2 = do input <- readFile "data7.txt"
             let rules = makeRules input
                 problemContent = (1,problemBag)
                 total = totalContentCount problemContent rules
             print total

answer :: IO ()
answer = do input <- readFile "data7.txt"
            let rules = makeRules input
                allbags = filter (/= problemBag) $ map fst rules
                allColorsPerBag =  map (`allColors` rules) allbags
                nbags = length . filter (problemBag `elem`) $ allColorsPerBag
            print nbags

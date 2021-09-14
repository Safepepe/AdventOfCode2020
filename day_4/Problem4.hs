module Problem4 where
import Control.Monad.State.Lazy

type Input = String
type Key = String
type Value = String
type Passport = [(Key,Value)]
type ParseData = State [Passport]  -- State s a
type Condition = Passport -> Bool


{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
                            {- Actualy program -}
{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
exerciceInput = readFile "data4.txt"

answer = exerciceInput >>= (print . length .filter valid . completePassports)


{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
                        {- Parsing input -}
{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
getPassport :: Input -> String
getPassport = takeWhile2 (/='\n')

dropPassport :: Input -> Input
dropPassport = dropWhile (=='\n') . dropWhile2 (/='\n')

parsePassports :: Input -> ParseData Input
parsePassports input = if length input > 0 then
                        do
                         stack <- get
                         let passport = makePassport $ getPassport input
                             rest = dropPassport input
                         put $ passport:stack
                         parsePassports rest
                       else return ""

makePassport :: String -> Passport
makePassport  = map makeValue . words
   where makeValue str = (takeWhile (/=':') str, tail . dropWhile (/=':') $ str)

keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
necessaryKeys = init keys

complete :: Condition
complete psport = and $ map (`elem` fields) necessaryKeys
  where fields = map fst psport

completePassports :: Input -> [Passport]
completePassports input  =  filter complete $ execState (parsePassports input) []


{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
                          {- Conditions for each field-}
{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
valid :: Condition
valid pport = and $ map ($pport) [byr, eyr, iyr, hgt, hcl, ecl, pid]


byr :: Condition
byr = inRange 1920 2002 . read . getKey "byr"
iyr :: Condition
iyr = inRange 2010 2020 . read . getKey "iyr"
eyr :: Condition
eyr = inRange 2020 2030 . read . getKey "eyr"
hgt :: Condition
hgt pport = if inInches then
             inRange 59 76 height
            else if inCm then
             inRange 150 193 height
            else False
      where height = (read . takeWhile isNum . getKey "hgt" $ pport ) :: Int
            unit = dropWhile isNum . getKey "hgt" $ pport
            inInches = "in" == unit
            inCm = "cm" == unit
hcl :: Condition
hcl = isHairColor . getKey "hcl"
ecl :: Condition
ecl = isEyeColor . getKey "ecl"
pid :: Condition
pid pport = length pnumber == 9 && justDigits pnumber
  where justDigits = and . map isNum
        pnumber = getKey "pid" pport


{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
{-    --    --    --    --    Helper functions  --    --    --    --    --    -}
{-    --    --    --    --    --    --    --    --    --    --    --    --    -}
takeWhile2 :: (Char -> Bool) -> String -> String
takeWhile2 _ []  = []
takeWhile2 _ [x] = [x]
takeWhile2 cond (x:y:str)
          | not (cond x || cond y) = []
          | otherwise        = x:(takeWhile2 cond (y:str))

dropWhile2 :: (Char -> Bool) -> String -> String
dropWhile2 _ [] = []
dropWhile2 _ [x] = []
dropWhile2 cond (x:y:str)
      | not (cond x || cond y) = x:y:str
      | otherwise              = dropWhile2 cond (y:str)

getKey :: String -> Passport -> String
getKey key = snd . head . dropWhile (\x -> (fst x) /= key)

isNum :: Char -> Bool
isNum = (`elem` "01233456789")

isHairColor :: String -> Bool
isHairColor str =   (head str == '#')
                    && (length rest == 6)
                    && (and $ map (`elem` alphabet) rest)
    where rest = tail str
          alphabet = "0123456789abcdef"

isEyeColor :: String -> Bool
isEyeColor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

inRange :: Int -> Int -> Int -> Bool
inRange lower upper n = n >= lower && n <= upper

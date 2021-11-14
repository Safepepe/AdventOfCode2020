module Problem4 where
import Data4 (inputIO)

{- Part One -}
type Value = String
type Entry = [(Key,Value)]
type Condition = Entry -> Bool
{-Entries can contain some of the following data
  byr (Birth Year)
  iyr (Issue Year)
  eyr (Expiration Year)
  hgt (Height)
  hcl (Hair Color)
  ecl (Eye Color)
  pid (Passport ID)
  cid (Country ID) -}
necessaryKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasNecessaryKeys :: Condition
hasNecessaryKeys entry = and $(`elem` fields)<$>necessaryKeys
  where fields = map fst entry

answer1 :: IO Int
answer1 = do
  entrs <- inputIO
  let passports = filter hasNecessaryKeys entrs
  return $ length passports

{-Part Two-}
isValid :: Condition
isValid pport = and $ map ($pport) [byr, eyr, iyr, hgt, hcl, ecl, pid]

byr :: Condition
byr = inRange 1920 2002 . read . getValue "byr"
iyr :: Condition
iyr = inRange 2010 2020 . read . getValue "iyr"
eyr :: Condition
eyr = inRange 2020 2030 . read . getValue "eyr"
hgt :: Condition
hgt pport = if inInches then
             inRange 59 76 height
            else if inCm then
             inRange 150 193 height
            else False
      where height = (read . takeWhile isNum . getValue "hgt" $ pport ) :: Int
            unit = dropWhile isNum . getValue "hgt" $ pport
            inInches = "in" == unit
            inCm = "cm" == unit
hcl :: Condition
hcl = isHairColor . getValue "hcl"
ecl :: Condition
ecl = isEyeColor . getValue "ecl"
pid :: Condition
pid pport = length pnumber == 9 && justDigits pnumber
  where justDigits = and . map isNum
        pnumber = getValue "pid" pport

answer2 :: IO Int
answer2 = do
  entrs <- inputIO
  let passports = filter isValid . filter hasNecessaryKeys $ entrs
  return $ length passports

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

getValue :: Key -> [(Key,a)] -> a
getValue key = snd . head . dropWhile (\x -> (fst x) /= key)

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

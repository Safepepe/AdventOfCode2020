module Problem6 where
import Data6 (inputIO)
import Data.List (sort)
import GHC.Exts (sortWith)

type Person = String
type Group = [Person]

{-Part One-}
yesFromAnyone :: Group -> String
yesFromAnyone = foldr addTo [] . concat
 where addTo x xs = if elem x xs then xs else x:xs

answer1 :: IO Int
answer1 = do
  grps <- inputIO
  return $ sum $ (length.yesFromAnyone)<$>grps

{-Part Two-}
yesFromEveryone :: Group -> String
yesFromEveryone grp = filter restSaidYes fstPerson
   where orderedGroup = map sort . sortWith (length) $ grp
         fstPerson = head orderedGroup
         rest        = tail orderedGroup
         restSaidYes x = and $ (elem x)<$>rest

answer2 :: IO Int
answer2 = do
  grps <- inputIO
  return $ sum $ (length.yesFromEveryone)<$>grps

module Problem2 where
import Data2 (inputIO)


{- Part One -}
type Entry = (Int,Int,Char,String)
type Policy=  Entry -> Bool

policyOne :: Policy
policyOne (mini,maxi,ch,str) = apply (mini,maxi,ch) str
  where apply (p, q, c) = (inRangeOf p q) . length . filter (== c)
        inRangeOf p q n = n >= p && n <= q

answer1 :: IO Int
answer1 = do
  entries <- inputIO
  return.length$ filter policyOne entries

{-Part Two-}
policyTwo :: Policy
policyTwo (mini,maxi,ch,str) = fstPos `xorMaybe` sndPos
 where fstPos = if length str >= mini then Just (str!!(mini-1) == ch) else Nothing
       sndPos = if length str >= maxi then Just (str!!(maxi-1) == ch) else Nothing
       xorMaybe a b = (toBool a) `xor` (toBool b)
       toBool (Just b) = b
       toBool Nothing  = False
       xor a b = (a && not b) || (not a && b)

answer2 :: IO Int
answer2 = do
  entries <- inputIO
  return.length$ filter policyTwo entries

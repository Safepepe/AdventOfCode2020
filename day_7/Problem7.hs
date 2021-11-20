module Problem7 where
import Data7 (inputIO)
import Data.List (partition)
type Container = String
type Content = (Int, Container)
type Rule = (Container, [Content])


{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
exampleBag = "mirrored magenta"--"shiny gold"

nextBags:: Container -> [Rule] -> [Container]
nextBags cont rules  = map snd itsContents
     where itsRules    = filter ((==cont).fst) rules
           itsContents = concat . map snd $ itsRules

unfold :: (a -> Maybe (b,a)) -> a -> [b]
unfold f x = case f x of
              Just (y,seed) -> y: unfold  f seed
              Nothing       -> []

mergeWith :: Eq a => [a] -> [a] -> [a]
mergeWith xs stack = foldr (\x s-> if elem x s then s else x:s) stack xs

{- functions used in brute force approach -}
allBagsIn :: Container -> [Rule] -> [Container]
allBagsIn bag rules = foldr mergeWith [] $ unfold nextBagsMaybe [bag]
  where nextBagsMaybe :: [Container] -> Maybe ([Container],[Container])
        nextBagsMaybe bgs = let nbgs = concat $ (`nextBags` rules)<$> bgs in
                       if length nbgs > 0 then
                         Just (nbgs,nbgs)
                       else
                         Nothing
eventuallyContains :: Container -> Container -> [Rule] -> Bool
eventuallyContains containerBag contentBag rules  = contentBag `elem` allBagsIn containerBag rules
{- functions used in smarter approach -}
containsBag :: Container -> Container -> [Rule] -> Bool
containsBag containerBag contentBag rules  = contentBag `elem` (nextBags containerBag rules)

previousBags :: Container -> [Rule] -> [Container]
previousBags bag0 rules = filter (`contains0`bag0) allBags
  where contains0 x y = containsBag x y rules
        allBags = fst <$> rules

bagsEventuallyContaining :: Container -> [Rule] -> [Container]
bagsEventuallyContaining bag0 rules = foldr mergeWith [] $ unfold previousBagsMaybe [bag0]
  where previousBagsMaybe :: [Container] -> Maybe ([Container], [Container])
        previousBagsMaybe bgs = let pbgs = concat $ (`previousBags` rules)<$>bgs in
                                if length pbgs > 0 then
                                  Just (pbgs,pbgs)
                                else
                                  Nothing
{-=-----------------------------------=-}
answer1 :: IO Int
answer1 = do
  rules <- inputIO
  let bags = fst <$> rules
      bagsContainingBrute exbg = filter (\x -> (x`eventuallyContains`exbg)rules) bags --Brute force approach
      bagsContainingSmart exbg = bagsEventuallyContaining exbg rules --Smarter approach
  return $ length $ bagsContainingSmart exampleBag
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
{- We could use the State monad, however we won't as the "unfold" function from
   Part One will suffice for our purposes. -}
   
nextContentsOf :: Container -> [Rule] -> [Content] --Unsafe because of head.
nextContentsOf bag = snd . head . filter ((== bag).fst)

addUpBags :: [Content] -> [Content]
addUpBags  = foldr searchAndAdd []
 where
   searchAndAdd :: Content -> [Content] -> [Content]
   searchAndAdd (n,bg) cs =
     let (cnts, rest) = partition ((==bg).snd) cs
     in (sum (fst<$>cnts) + n, bg):rest

recursiveBags :: Container -> [Rule] -> [[Content]]
recursiveBags bag0 rules = unfold nextContentsOfMaybe [(1,bag0)]
 where
   multiplyThrough :: Content -> [Content] -> [Content]
   multiplyThrough (n,bg) ys = (\(p,q) -> (p*n, q)) <$> ys
   noNew :: [[a]] -> Bool
   noNew = null . filter (not.null)
   nextContentsOfMaybe :: [Content] -> Maybe ([Content], [Content])
   nextContentsOfMaybe conts =
         let cntntsPerBag = ((`nextContentsOf`rules).snd)<$> conts --[[Content]]
             newcnts = addUpBags.concat $ zipWith multiplyThrough conts cntntsPerBag
         in if noNew cntntsPerBag then Nothing else Just (newcnts,newcnts)

totalBagsInsideOf :: Container -> [Rule] -> Int
totalBagsInsideOf bag0 = sum.map fst.concat.recursiveBags bag0

{-=-----------------------------------=-}
answer2 :: IO Int
answer2 = do
  rules <- inputIO
  return $ totalBagsInsideOf exampleBag rules

module Problem20 where
import Data20 (inputIO)

type Image = [String]
showFunc :: Image -> IO ()
showFunc []     = return ()
showFunc (x:xs) = do putStrLn.concat$(:" ")<$>x
                     showFunc xs
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
data Tree a = Node a [Tree a]
  deriving(Show, Eq)

fitTogether :: Tile -> Tile -> Bool
fitTogether (id1,im1) (id2,im2) =  or$(==)<$>potBorders<*>borders im2
  where
    borders img = head img : last img : (head<$>img) : [(last<$>img)]
    potBorders = borders im1 ++ (reverse<$>borders im1)

unfold :: (a -> Maybe (b,a)) -> a -> [b]
unfold f x = case f x of
              Just (y,seed) -> y: unfold  f seed
              Nothing       -> []

attachStep :: (Tree Tile, [Tile]) -> Maybe (Tree Tile, [Tile])
attachStep (_, [])    = Nothing
attachStep (t , rest) = if length newRest < length rest then
                            Just (foldr tMerge t nSubTrees, newRest)
                        else
                            Nothing
  where
    (nSubTrees,newRest) = foldr attachToLeaf (leafs t, []) rest
{-
tFit :: Tree Tile -> Tile -> Tree Tile
tFit (Node tl1 ts) tl2 = if tl1`fitsWith`tl2 then
                           Node tl1 ((Node tl2 []):ts)
                         else
                           Node tl1 ts
-}


leafs :: Tree a -> [Tree a]
leafs (Node x []) = [Node x []]
leafs (Node x ts) = concat$leafs<$>ts

tMerge :: Eq a => Tree a -> Tree a -> Tree a
tMerge greatT      (Node y [])  = greatT
tMerge (Node x []) (Node y yts) = if x == y then Node y yts else Node x []
tMerge (Node x ts)  t1          = Node x ((`tMerge`t1)<$>ts)

attachToLeaf :: Tile ->([Tree Tile], [Tile]) -> ([Tree Tile], [Tile])
attachToLeaf tl ([], rest) = ([],tl:rest)
attachToLeaf tl (ts, rest) = let (nts,usedTl) = foldr (accumFit tl) ([],False) ts in
                              if usedTl then (nts,rest) else (ts, tl:rest)

{-
rotateCW :: Image -> Image
rotateCW [] = []
rotateCW xs = map reverse $ foldr (zipWith (:)) emptys xs
  where
    emptys = replicate (length.head$xs) []

rotateCCW :: Image -> Image
rotateCCW [] = []
rotateCCW xs = reverse $ foldr (zipWith (:)) emptys xs
  where
    emptys = replicate (length.head$xs) []

flipX :: Image -> Image
flipX = map reverse

flipY :: Image -> Image
flipY = reverse
-}

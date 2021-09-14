module Problem15 where
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as M

type Dictionary = M.IntMap Position
type Position = Int
type Value = Int
type Current = (Value,Position)
type GameState2 = State (Current, Dictionary)

turnNumber2 = 30000000

takeTurn2 :: GameState2 ()
takeTurn2 = do ((key,pos), dictio) <- get
               if  M.member key dictio then
                 do let (Just pastPos) = M.lookup key dictio
                        newDictionary = M.insert key pos dictio
                    put ((pos-pastPos, pos+1), newDictionary)
               else
                 put $ ((0,pos+1), (M.insert key pos dictio))

loopUntil ::  GameState2 Value
loopUntil = do ((k,pos), dictio) <- get
               if pos >= turnNumber2 then
                  return k
               else
                  do takeTurn2
                     loopUntil

answer2 :: IO Int
answer2 = do ((k,val),list0) <- input2
             let listSize     = length list0 +1
                 nTurnsToTake = max 0 (turnNumber2 - listSize)
                 v            = evalState loopUntil ((k,val), M.fromList list0)
             return v

input2 :: IO ((Value,Position), [(Value,Position)])
input2 = do str <- readFile "data15.txt"
            let startingList = reverse . (`zip` [1..])  $ map read . lines $ str
                current = head startingList
                dictio  = tail startingList
            return $ (current, dictio)

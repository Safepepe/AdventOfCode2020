module Problem13 where
import Parsing
import Control.Monad.State.Lazy
{- Part 1 -}

{- Parsing bit of code -}
type TimeStamp = Int
type BusID = Int

busIDsParser :: Parser [BusID]
busIDsParser = some busID
   where busID = do many $ char ',' <|> char 'x'
                    n <- nat
                    return n

input1 :: IO (TimeStamp, [BusID])
input1 = do str <- readFile "data13.txt"
            let lns = lines str --only 2 lines
                timeStamp0 = read (lns!!0) :: Int
                buses = fst.head$parse busIDsParser (lns!!1)
            return (timeStamp0, buses)

{- General bit of code -}

nextTime :: TimeStamp -> BusID -> Int
nextTime t0 n
       | (t0 `mod` n) == 0 = t0
       | otherwise         = (t0`div`n +1)*n

nearestBusID :: TimeStamp -> [BusID] -> BusID
nearestBusID t0 bIDs = foldr1 (compareTime t0) bIDs
   where compareTime t b1 b2 = if nextTime t b1 > nextTime t b2 then b2 else b1

answer1 :: IO Int
answer1 = do (t0,bIDs) <- input1
             let closestBus = nearestBusID t0 bIDs
                 waitTime = nextTime t0 closestBus - t0
             return $ closestBus*waitTime

{- Part 2 -}

type WaitTime = Int
{-Three stages :
  1) solve the case of 2 buses (Bezout's identity)
  2) solve the induction case
  3) solve the general problem and verify that it's in fact the smalles solution-}

  {-Preliminary work: Extended Euclidean algorithm -}
  {-Using the notation from wikipedia -}
remainders :: Int -> Int -> [Int]
remainders a b = if abs(a) > abs(b) then
                   (abs a):(abs b):remaindersAfter (abs a) (abs b)
                 else
                   (abs b):(abs a):remaindersAfter (abs b) (abs a)
  where remaindersAfter a b  -- Assumption: a >= b > 0
            | b/=0 && a`mod`b /= 0 = (a`mod`b):(remaindersAfter b (a`mod`b))
            | otherwise            = []

divisors :: [Int] -> [Int]
divisors rs = zipWith (div) rs (tail rs)

bezoutCoeffs :: Int -> Int -> (Int, Int)
bezoutCoeffs a b = if abs a > abs b then (sk,tk) else (tk,sk)
  where divisorList = divisors $ remainders a b
        t = [1,0] --t1,t0
        s = [0,1] --s1,s0
        tk = head$tList t divisorList
        sk = head$sList s divisorList
        sList ss qs = if length ss > length qs then
                        ss
                      else
                        let i  = length ss
                            si = ss!!1 - (qs!!(i-2))*(ss!!0)
                        in sList (si:ss) qs
        tList ts qs = if length ts > length qs then
                          ts
                      else
                          let i  = length ts
                              ti = ts!!1 - (qs!!(i-2))*(ts!!0)
                          in tList (ti:ts) qs
{- solve for 2 buses-}
solutionFor2 :: (BusID, WaitTime) -> (BusID, WaitTime) -> TimeStamp
solutionFor2 (a0,b0) (a1,b1) = (b1*a0*m0 + b0*a1*m1)`mod`lcm a0 a1
  where (m0,m1) = bezoutCoeffs a0 a1

{- Now we solve for n+1 buses-}
addCondition :: (BusID,WaitTime) -> State (TimeStamp, [(BusID,WaitTime)]) ()
addCondition (b1,w1) = do (t0, bws) <- get
                          if length bws == 0 then
                           do let t0 = (w1+ b1 + ((abs w1)`div`(abs b1))*b1)`mod`b1
                              put (t0, (b1,w1):bws)
                          else
                           if length bws == 1 then
                              do let t1 = solutionFor2 (b1,w1) (head bws)
                                 put (t1, (b1,w1):bws)
                           else
                             do let newCond k = (k-w1)>0 && (k-w1)`mod`b1 ==0
                                    pastLCM = foldr1 (*) . map fst $ bws
                                    t1List = map (\k-> t0 + k*pastLCM) [0..b1]
                                    t2 = head $ filter newCond $ t1List
                                put (t2,(b1,w1):bws)

{-Answer to problem-}

initState2 :: (TimeStamp,[(BusID,WaitTime)])
initState2 = (1,[])

answer2 :: IO TimeStamp
answer2 = do bws <- input2
             let (t0,bwlist) = execState (mapM_ addCondition bws) initState2
             print.reverse$bwlist
             return t0
{- Parsing bit of code -}
busIDsParser2 :: Parser [BusID]
busIDsParser2 = some $ busID
  where busID = do many $ char ','
                   n <- nat <|> xParser
                   many $ char ','
                   return n
        xParser = char 'x' >> return 1

input2 :: IO [(BusID,WaitTime)]
input2 = do str <- readFile "data13.txt"
            let ln2     = (lines str)!!1
                busList0 = fst.head$parse busIDsParser2 ln2
                busList = filter ((/=1).fst) $ zip busList0 [0..(length busList0 -1)]
            return $ map (\(x,y)-> (x,-y)) busList

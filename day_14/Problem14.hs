module Problem14 where
import Parsing
import Data.Word -- for working with unsigned integers
import Data.Bits -- where the Bits typeClass is defined
import Control.Monad.State.Lazy


{- tools -}
maskOnes :: [Int] -> MaskBit
maskOnes pos xByte = foldr (flip setBit) xByte pos

maskZeros :: [Int] -> MaskBit
maskZeros pos xByte = foldr (flip clearBit) xByte pos

--gives "positions" of elements in a list verifying a condition (sub-optimal)
takePos :: (a-> Bool) -> [a] -> [Int]
takePos  _ []    = []
takePos cond (x:xs) = if cond x then
                       (length xs): takePos cond xs
                      else
                        takePos cond xs
{-+++===++=+=+=+++=+==+++++++===++== Part 2 +++===+++==+=+=+++=+==+++++++===+=-}
type Address2 = Word64
type Value2 = Word64
type MaskBit2 = Address2 -> [Address2]
type DataBase2 = State (MaskBit2,[(Address2,Value2)])
type Command2 =  DataBase2 ()

initDB2 = ((:[]),[]) --no mask and no data in the database

input2 :: IO [Command2]
input2 = do str <- readFile "data14.txt"
            let cmds = map (fst.head.parse cmdP2) $ lines str
            return cmds

answer2 :: IO Value
answer2 = do cmds <- input2
             let (msk,db) = execState (sequence cmds) initDB2
                 total = sum . map snd $ db
             --print db
             return total

{-+++==+++=+=+++++===+= Parsing bit of code (Part 2) +++===++=+=++=====++===+=-}
cmdP2 :: Parser Command2
cmdP2 = memP2 <|> maskP2

maskP2 :: Parser Command2
maskP2 = do string "mask"
            space
            char '='
            space
            str <- some $ char 'X' <|> char '1' <|> char '0'
            return $ get >>= (\(_,db) -> put ((stringToMasks2 str),db))
  where stringToMasks2 :: String -> MaskBit2
        stringToMasks2 str adrs = foldr applyX [newAdrs] xPos
         where onePos  = takePos (=='1') str --bits to chane to 1
               newAdrs = maskOnes onePos adrs --newAddress BEFORE applying the 'X's
               xPos    = takePos (=='X') str --bits to apply the X
               applyX bitPos wrds = ((`setBit`bitPos)<$>wrds)++((`clearBit`bitPos)<$>wrds)

memP2 :: Parser Command2
memP2 = do string "mem"
           char '['
           adrsInt <- nat
           char ']'
           space
           char '='
           space
           n <- nat
           let adrs = fromIntegral adrsInt
               nVal = fromIntegral n
           return $ (putInMemory adrs nVal)
  where putInMemory :: Address2 -> Value2 -> Command2
        putInMemory adr val = do (msk,db) <- get
                                 let adrs    = msk adr
                                     entries = zip adrs (repeat val)
                                 put (msk, entries ++ filter (not.(`elem`adrs).fst) db)
{-+++===++=+=+=+++=+==+++++++===+= END OF PART 2 ++=++=+=+=+++=+==+++++++===+=-}
{-+++===++=+=+=+++=+==+++++++===+= Part 1 ++===++=+=+=+++=+=++=+==+++++++===+=-}
type Address = Word64
type Value = Word64
type MaskBit = Value -> Value
type DataBase = State (MaskBit,[(Address,Value)])
type Command=  DataBase ()

initDB = (id,[]) --no mask and no data in the database

input1 :: IO [Command]
input1 = do str <- readFile "data14.txt"
            let cmds = map (fst.head.parse cmdP) $ lines str
            return cmds

answer1 :: IO Value
answer1 = do cmds <- input1
             let (msk,db) = execState (sequence cmds) initDB
                 total = sum . map snd $ db
             --print db
             return total



{-+++===++=+=+=+++=+==++ Parsing bit of code (Part 1) +++==+++=+==+++++++===+=-}
cmdP :: Parser Command
cmdP = memP <|> maskP

maskP :: Parser Command
maskP = do string "mask"
           space
           char '='
           space
           str <- some $ char 'X' <|> char '1' <|> char '0'
           return $ get >>= (\(_,db) -> put ((stringToMask str),db))
  where stringToMask :: String -> MaskBit
        stringToMask str = maskOnes onePos . maskZeros zeroPos
          where zeroPos = takePos (=='0') str
                onePos  = takePos (=='1') str

memP :: Parser Command
memP = do string "mem"
          char '['
          adrsInt <- nat
          char ']'
          space
          char '='
          space
          n <- nat
          let adrs = fromIntegral adrsInt
              nVal    = fromIntegral n
          return $ (putInMemory adrs nVal)
  where putInMemory :: Address -> Value -> Command
        putInMemory ads val = do (msk,db) <- get
                                 put  (msk, (ads, msk val):(filter ((/= ads).fst) db))

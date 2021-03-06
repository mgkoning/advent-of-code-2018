import Prelude hiding (lookup)
import Data.Bits
import Data.IntMap.Strict (IntMap, (!), insert, elems, fromList, lookup)
import qualified Data.IntMap.Strict as IntMap
import Data.Char (toUpper)
import Text.Parsec (parse, getInput, setInput, (<|>), sepEndBy, many1, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, letter, spaces)
import Control.Applicative (empty)
import Numeric (readDec, readSigned)

data State = State { registry :: Registry, program :: Program, instructionPointer :: Int, boundRegister :: Int } deriving (Show)

type Registry = IntMap Int
type Program = IntMap Instruction

data Opcode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori {- binary -}
            | Setr | Seti {- assignment -}
            | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr {- testing -}
            deriving (Show, Read, Eq, Enum, Bounded)

data Instruction = Instruction Opcode Int Int Int deriving (Show)

(-:) x f = f x

storeIn registry loc val = insert loc val registry

readFrom registry loc = registry ! loc

enumerated :: [a] -> [(Int, a)]
enumerated = zip [0..]

emptyRegistry = (fromList $ zip [0..5] (repeat 0))

run :: Registry -> Instruction -> Registry
run r (Instruction opcode arg0 arg1 result) = storeIn r result $ computation
  where reg0 = readFrom r arg0
        reg1 = readFrom r arg1
        val0 = arg0
        val1 = arg1
        binaryR op = op reg0 reg1
        binaryI op = op reg0 val1
        fromBool x = if x then 1 else 0
        testIR op = fromBool $ op val0 reg1
        testRI op = fromBool $ op reg0 val1
        testRR op = fromBool $ op reg0 reg1
        computation = case opcode of
                        Addr -> binaryR (+)
                        Addi -> binaryI (+)
                        Mulr -> binaryR (*)
                        Muli -> binaryI (*)
                        Banr -> binaryR (.&.)
                        Bani -> binaryI (.&.)
                        Borr -> binaryR (.|.)
                        Bori -> binaryI (.|.)
                        Setr -> reg0
                        Seti -> val0
                        Gtir -> testIR (>)
                        Gtri -> testRI (>)
                        Gtrr -> testRR (>)
                        Eqir -> testIR (==)
                        Eqri -> testRI (==)
                        Eqrr -> testRR (==)

runProgram state = case next of Nothing -> state; Just i -> runProgram $ runIt i
  where State reg program ip boundRegister = state
        next = lookup ip program
        runIt instruction = state { instructionPointer = nextIp, registry = reg' }
          where reg' = run (storeIn reg boundRegister ip) instruction
                nextIp = (readFrom reg' boundRegister) + 1
        

solve = do
  state <- parseProgram <$> readFile "input.txt"
  putStrLn "Part 1:"
  let part1 = runProgram state
  print $ (registry part1) ! 0
  {- Can't really make a general solution for all inputs, but:
     The program in my input takes a certain number and sums all 
     the factors that number has. Part 1 uses a prime and so the output
     is the number + 1; part 2 uses a non-prime (in my case 10551277) and so
     sadly the answer to part 2 is not the number + 1, but we can optimize 
     the function to calculate the factors somewhat: we can skip 1 and n
     (they're always factors, unless the number is 1) and only need to check
     [2..(half n)].
  -}
  putStrLn "Part 2:"
  print $ factorSum 10551277

factorSum n = if n == 1 then 1 else 1 + n + (sum $ filter ((==0) . (mod n)) [2..(n `div` 2)])

parseProgram :: String -> State
parseProgram input = State emptyRegistry (program -: enumerated -: fromList) 0 ip
  where (ip, program) = case (parse parseProgram' "" input) of
                          Left msg -> error (show msg)
                          Right r -> r
        parseProgram' = (,) <$ string "#ip " <*> parseInt <* eol <*> parseInstruction `sepEndBy` eol <* eof

parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseOp <*> parseInt <* spaces <*> parseInt <* spaces <*> parseInt

parseOp :: Parser Opcode
parseOp = mkOpcode <$> many1 letter
  where mkOpcode op = (read $ (toUpper $ head op):(drop 1 op))

eol :: Parser String
eol = string "\r\n" <|> string "\n"

parseInt :: Parser Int
parseInt = do s <- getInput
              case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
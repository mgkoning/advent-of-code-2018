import Prelude hiding (lookup)
import Data.Bits
import Data.IntMap.Strict (IntMap, (!), insert, elems, fromList, lookup)
import qualified Data.IntMap.Strict as IntMap
import Data.Char (toUpper)
import Debug.Trace (trace)

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

readInstruction line = Instruction opcode a b c
  where (op:rest) = words line
        (a:b:c:[]) = map read rest
        opcode = read $ (toUpper $ head op):(drop 1 op)

readProgram input = State emptyRegistry program 0 ip
  where inputLines = lines input
        ip = inputLines -: head -: drop 4 -: read
        program = inputLines -: drop 1 -: map readInstruction -: enumerated -: fromList

runProgram state = case next of Nothing -> state; Just i -> runProgram $ runIt i
  where State reg program ip boundRegister = state
        next = lookup ip program
        runIt instruction = {- trace ((show nextIp) ++ ": " ++ (show reg')) $ -} state { instructionPointer = nextIp, registry = reg' }
          where reg' = run (storeIn reg boundRegister ip) instruction
                nextIp = (readFrom reg' boundRegister) + 1
        

solve = do
  state <- readProgram <$> readFile "input.txt"
  putStrLn "Part 1:"
  let part1 = runProgram state
  print $ (registry part1) ! 0
  --putStrLn "Part 2:"
  --let part2 = runProgram (state { registry = insert 0 1 (registry state)})
  --print $ (registry part2) ! 0

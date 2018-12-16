import Data.Bits
import Data.IntMap.Strict (IntMap, (!), insert, elems, fromList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Data.List (foldl', nub, (\\))

type Registry = IntMap Int

data Opcode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori {- binary -}
            | Setr | Seti {- assignment -}
            | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr {- testing -}
            deriving (Show, Read, Eq, Enum, Bounded)

allOpcodes :: [Opcode]
allOpcodes = enumFrom minBound

data Instruction = Instruction Opcode Int Int Int deriving (Show)

data IntInstruction = IntInstruction Int Int Int Int deriving (Show)

storeIn registry loc val = insert loc val registry

readFrom registry loc = registry ! loc

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

(-:) x f = f x

readSamples :: String -> [(Registry, IntInstruction, Registry)]
readSamples input = lines input -: chunksOf 4 -: map readSample

readSample :: [String] -> (Registry, IntInstruction, Registry)
readSample (before:instruction:after:_) = (readRegistry before, readIntInstruction instruction, readRegistry after)
  where readRegistry line = IntMap.fromList $ zip [0..] (read (drop 8 line) :: [Int])
readSample _ = error "not enough lines"

readIntInstruction instruction = let (a:b:c:d:_) = map read $ words instruction in IntInstruction a b c d

chunksOf n [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in [chunk] ++ (chunksOf n rest)

allOpcodeMatches = opcodeMatches allOpcodes

zipMap f xs = zip xs (map f xs)

opcodeMatches opcodes sample = filter snd $ zipMap (isMatch sample) opcodes

isMatch (before, IntInstruction _ a b c, after) opcode =
  after == (run before (Instruction opcode a b c))

buildOpcodeMap samples = buildOpcodeMap' (IntMap.empty) allOpcodes
  where buildOpcodeMap' builtMap [] = builtMap
        buildOpcodeMap' builtMap remainingCodes =
          let singleMatches = filter ((==1) . length . snd) $ zipMap (opcodeMatches remainingCodes) samples
              extractOpcodeMapping ((_, IntInstruction intCode _ _ _, _), matchingOpcodes) = (intCode, fst $ head matchingOpcodes)
              unambiguousOpcodes = nub $ map extractOpcodeMapping singleMatches
              builtMap' = foldr (\(k, v) -> insert k v) builtMap unambiguousOpcodes
          in 
            if null singleMatches
              then error "Can't unambiguize"
              else buildOpcodeMap' builtMap' (remainingCodes \\ (map snd unambiguousOpcodes))

part1 samples = foldr matches3OrMore 0 samples
  where matches3OrMore sample acc = acc + if 2 < (length $ allOpcodeMatches sample) then 1 else 0

part2 opcodeMap program = foldl' runInstruction emptyRegistry program
  where runInstruction r (IntInstruction op a b c) = run r (Instruction (opcodeMap ! op) a b c)
        emptyRegistry = (fromList $ zip [0..3] (repeat 0))

solve = do
  putStrLn "Part 1:"
  samples <- readSamples <$> readFile "input1.txt"
  putStrLn $ show $ part1 samples

  let opcodeMap = buildOpcodeMap samples
  program <- map (readIntInstruction) <$> lines <$> readFile "input2.txt"
  let result = part2 opcodeMap program
  putStrLn "Part 2:"
  putStrLn $ show $ (result ! 0)

testSamples = unlines [
  "Before: [3, 2, 1, 1]",
  "9 2 1 2",
  "After:  [3, 2, 2, 1]"]

testSample = head $ readSamples $ testSamples
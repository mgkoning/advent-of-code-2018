import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Function (on)

readInput :: [String] -> (Set Int, Map String Char)
readInput lines = (makeInitialState $ drop 15 (head lines), Map.fromList $ map readEvolution (drop 2 lines))

readEvolution line = (take 5 line, head $ drop 9 line)

makeInitialState = Set.fromAscList . map fst . filter (((==) '#') . snd) . zip [0..]

evolve notes state = Set.fromList $ catMaybes $ map (grow notes state) [(Set.findMin state)-3..(Set.findMax state)+3]

grow notes state x =
   let window = showPlants state [x-2..x+2]
   in if (Map.lookup window notes) == (Just '#') then Just x else Nothing

showPlants state indices = map (\x' -> if x' `Set.member` state then '#' else '.') indices

showAllPlants state = showPlants state [(Set.findMin state)..(Set.findMax state)]

duplicatingFrom eq (a:b:rest) = if eq a b then a else duplicatingFrom eq (b:rest)

solve = do
  (initialState, notes) <- readInput <$> lines <$> readFile "input.txt"
  let iteratedEvolution = iterate (evolve notes) initialState
  let sumAtEvolution x = sum $ head $ drop x $ iteratedEvolution
  putStrLn "Part 1:"
  putStrLn $ show $ sumAtEvolution 20
  putStrLn "Part 2:"
  {-
    From a certain point, the difference in sums between different evolutions
    doesn't seem to change, looking at the series. Find the first repeated
    difference sum and calculate sum after 50 BILLION using the index, the sum
    and the repeated difference.
  -}
  let sums = map sum iteratedEvolution
  let differences = zipWith (-) (drop 1 sums) sums
  let (repeatingFrom, repeatingSum) = duplicatingFrom ((==) `on` snd) $ zip [0..] differences
  putStrLn $ show $ (sumAtEvolution repeatingFrom) + (repeatingSum * (50000000000 - repeatingFrom))

(testInitialState, testNotes) = readInput $ lines $ "initial state: #..#.#..##......###...###\n\n" ++
  "...## => #\n" ++
  "..#.. => #\n" ++
  ".#... => #\n" ++
  ".#.#. => #\n" ++
  ".#.## => #\n" ++
  ".##.. => #\n" ++
  ".#### => #\n" ++
  "#.#.# => #\n" ++
  "#.### => #\n" ++
  "##.#. => #\n" ++
  "##.## => #\n" ++
  "###.. => #\n" ++
  "###.# => #\n" ++
  "####. => #\n"
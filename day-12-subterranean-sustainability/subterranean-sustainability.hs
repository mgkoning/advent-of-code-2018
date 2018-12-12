import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Function (on)

readInput :: [String] -> (Set Int, Map String Char)
readInput lines = (makeInitialState $ drop 15 (head lines), Map.fromList $ map readNote (drop 2 lines))

readNote :: String -> (String, Char)
readNote line = (take 5 line, head $ drop 9 line)

makeInitialState :: [Char] -> Set Int
makeInitialState = Set.fromAscList . map fst . filter (((==) '#') . snd) . zip [0..]

evolve :: Map String Char -> Set Int -> Set Int
evolve notes state = Set.fromList $ catMaybes $ map (grow notes state) [(Set.findMin state)-2..(Set.findMax state)+2]

grow :: Map String Char -> Set Int -> Int -> Maybe Int
grow notes state x =
   let window = showPlants state [x-2..x+2]
   in if (Map.lookup window notes) == (Just '#') then Just x else Nothing

showPlants :: Set Int -> [Int] -> String
showPlants state indices = map (\x' -> if x' `Set.member` state then '#' else '.') indices

showAllPlants :: Set Int -> String
showAllPlants state = showPlants state [(Set.findMin state)..(Set.findMax state)]

duplicatingFrom :: (a -> a -> Bool) -> [a] -> a
duplicatingFrom eq (a:b:rest) = if eq a b then a else duplicatingFrom eq (b:rest)

solve = do
  (initialState, notes) <- readInput <$> lines <$> readFile "input.txt"
  let iteratedEvolution = iterate (evolve notes) initialState
  let sums = map sum iteratedEvolution
  let sumAtEvolution x = head $ drop x $ sums
  putStrLn "Part 1:"
  putStrLn $ show $ sumAtEvolution 20
  putStrLn "Part 2:"
  {-
    From a certain point, the pattern repeats but shifts one to the right.
    I realized that by looking at the difference in sums between evolutions:
    it doesn't change from a certain point (the repeating pattern).

    The repeating pattern, for future reference:
    ##.#..##.#..##.#..##.#..##.#....##.#..##.#..##.#..##.#....##.#....##.#..##.#....##.#..##.#..##.#..##.#..##.#....##.#..##.#..##.#..##.#..##.#....##.#..##.#..##.#..##.#....##.#

    Find the first repeated sum difference and calculate sum
    after _50 BILLION_ using the index, the sum and the repeated difference.
  -}
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
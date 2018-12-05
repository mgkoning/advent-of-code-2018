import Data.List (sort, group, sortOn, sortBy)
import Data.Ord (Down(..), comparing)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

buildNodOffs :: [String] -> [(Int, [Int])]
buildNodOffs lines = buildNodOffs' lines 0 0 Map.empty
  where
    buildNodOffs' [] _ _ nodOffs = Map.toList nodOffs
    buildNodOffs' (line:lines) guardId fellAsleep nodOffs =
      let (firstword, newGuardId, minute) = getLineValues line
      in if firstword == "Guard" then
           buildNodOffs' lines newGuardId 0 nodOffs
         else if firstword == "falls" then
           buildNodOffs' lines guardId minute nodOffs
         else if firstword == "wakes" then
           buildNodOffs' lines guardId 0 $ Map.insertWith (++) guardId [fellAsleep..minute-1] nodOffs
         else error ("Unknown event: " ++ line)

getLineValues :: String -> (String, Int, Int)
getLineValues line = (firstword, newGuardId, minute)
  where packedLine = T.pack line
        (date:time:firstword':rest) = T.splitOn (T.pack " ") packedLine
        minute = read $ T.unpack (T.dropEnd 1 $ T.drop 3 time)
        newGuardId = read $ T.unpack $ T.drop 1 $ head rest
        firstword = T.unpack firstword'

sortDescendingOn :: Ord a => (b -> a) -> [b] -> [b]
sortDescendingOn f = sortBy $ comparing (Down . f)

sortMostAsleepAt :: Ord a => [a] -> [[a]]
sortMostAsleepAt = sortDescendingOn length . group . sort

part1 :: [(Int, [Int])] -> (Int, Int)
part1 nodOffs = (guardId, findMostAsleepAt sleepMinutes)
  where
    (guardId, sleepMinutes) = head $ sortDescendingOn mostAsleep nodOffs
    mostAsleep = length . snd
    findMostAsleepAt = head . head . sortMostAsleepAt

part2 :: [(Int, [Int])] -> (Int, Int)
part2 nodOffs = (guardId, head $ sleepMinutes)
  where
    (guardId, sleepMinutes) = head $ sortDescendingOn (length . snd) $ map determineMostSleepMinutes nodOffs
    determineMostSleepMinutes (guard, sleep) = (guard, head $ sortMostAsleepAt sleep)

solve = do
  input <- sort <$> lines <$> readFile "input.txt"
  let nodOffs = buildNodOffs input
  putStrLn "Part 1:"
  let (mostAsleep1, mostAsleepAt1) = part1 nodOffs
  putStrLn $ show $ mostAsleep1 * mostAsleepAt1

  putStrLn "Part 2:"
  let (mostAsleep2, mostAsleepAt2) = part2 nodOffs
  putStrLn $ show $ mostAsleep2 * mostAsleepAt2

{- Test stuff -}
testLines = ["[1518-11-01 00:00] Guard #10 begins shift",
  "[1518-11-01 00:05] falls asleep",
  "[1518-11-01 00:25] wakes up",
  "[1518-11-01 00:30] falls asleep",
  "[1518-11-01 00:55] wakes up",
  "[1518-11-01 23:58] Guard #99 begins shift",
  "[1518-11-02 00:40] falls asleep",
  "[1518-11-02 00:50] wakes up",
  "[1518-11-03 00:05] Guard #10 begins shift",
  "[1518-11-03 00:24] falls asleep",
  "[1518-11-03 00:29] wakes up",
  "[1518-11-04 00:02] Guard #99 begins shift",
  "[1518-11-04 00:36] falls asleep",
  "[1518-11-04 00:46] wakes up",
  "[1518-11-05 00:03] Guard #99 begins shift",
  "[1518-11-05 00:45] falls asleep",
  "[1518-11-05 00:55] wakes up"]

testNodOffs = buildNodOffs $ testLines

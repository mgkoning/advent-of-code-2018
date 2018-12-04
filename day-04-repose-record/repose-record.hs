import Data.List (sort, group, sortOn, groupBy)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Event = OnDuty String | Asleep Int | Awake Int deriving (Show, Eq)

data NoddedOff = NoddedOff { guard :: String, from :: Int, to :: Int } deriving Show

parseLine line =
  let
    packedLine = T.pack line
    (date:time:firstword:rest) = T.splitOn (T.pack " ") packedLine
    minute = read $ T.unpack (T.dropEnd 1 $ T.drop 3 time)
    event =
      case () of _
                  | firstword == (T.pack "Guard") -> OnDuty (T.unpack $ T.drop 1 $ head rest)
                  | firstword == (T.pack "falls") -> Asleep minute
                  | firstword == (T.pack "wakes") -> Awake minute
  in event

buildNodOffs events = buildNodOffs' events "unknown" 0 []
  where
    buildNodOffs' [] _ _ nodOffs =  reverse nodOffs
    buildNodOffs' (e:events) guardId fellAsleep nodOffs =
      case e of
        OnDuty g -> buildNodOffs' events g 0 nodOffs
        Asleep from -> buildNodOffs' events guardId from nodOffs
        Awake awokeAt -> buildNodOffs' events guardId 0 ((NoddedOff guardId fellAsleep awokeAt):nodOffs)

timeAsleep nodOffs = foldl addNodOff Map.empty nodOffs
  where addNodOff map nodOff = Map.insertWith (+) (guard nodOff) (to nodOff - from nodOff) map

getMinutes nodOff = [(from nodOff)..(to nodOff - 1)]

determineMostAsleepAt = (\x -> (head x, length x)) . head . reverse . sortOn length . group . sort

part1 nodOffs = (mostAsleep, mostAsleepAt)
  where 
    mostAsleep = Map.foldlWithKey (\curr guardId time -> if time > snd curr then (guardId, time) else curr) ("unknown", 0) $ timeAsleep nodOffs
    guardId = fst mostAsleep
    guardTimes = filter ((==guardId) . guard) nodOffs
    minutesSleeping = concatMap getMinutes guardTimes
    mostAsleepAt = fst $ determineMostAsleepAt minutesSleeping

part2 nodOffs = winner
  where
    nodOffsByGuard = groupBy (\a b -> guard a == guard b) $ sortOn guard nodOffs
    minutesByGuard = map (\list -> (guard $ head list, concatMap getMinutes list)) nodOffsByGuard
    mostAsleepByGuard = map (\(g, l) -> (g, determineMostAsleepAt l)) minutesByGuard
    winner = head $ reverse $ sortOn (snd . snd) mostAsleepByGuard

solve = do
  input <- map parseLine <$> sort <$> lines <$> readFile "input.txt"
  let nodOffs = buildNodOffs input
  putStrLn "Part 1:"
  let (mostAsleep, mostAsleepAt) = part1 nodOffs
  putStrLn $ show $ (read $ (fst mostAsleep)) * mostAsleepAt

  putStrLn "Part 2:"
  let winner = part2 nodOffs
  putStrLn $ show $ (read $ (fst winner)) * (fst $ snd $ winner)

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

testNodOffs = buildNodOffs $ map parseLine $ testLines

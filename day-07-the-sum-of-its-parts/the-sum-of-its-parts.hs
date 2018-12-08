import Data.Char (ord)
import Data.List (sort, nub, (\\), delete, intersect, partition)
import Data.Map.Strict (Map, fromList, keys, (!), elems)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

--Step L must be finished before step D can begin.
parseStep spec = (spec !! 5, spec !! 36)

noPrerequisite steps = (nub $ map fst steps) \\ (nub $ map snd steps)
allSteps steps = let (a, b) = unzip steps in nub $ a ++ b
getPrerequisites steps key = map fst $ filter ((==key) . snd) steps

getPrerequisiteMap steps = let keys = map snd steps in fromList $ zip keys $ map (getPrerequisites steps) keys

timeForStep base stepChar = base + 1 + (ord stepChar - ord 'A')

properOrder steps = determineOrder prerequisiteMap startAt []
  where startAt = noPrerequisite steps
        prerequisiteMap = getPrerequisiteMap steps
        determineOrder _ [] done = reverse done
        determineOrder prereqMap (a:available) done = determineOrder newMap (sort $ newAvailable ++ available) (a:done)
          where removedPrereq = Map.map (delete a) prereqMap
                newAvailable = keys $ Map.filter null removedPrereq
                newMap = Map.filterWithKey (\k _ -> not $ k `elem` newAvailable) removedPrereq

timeNeeded baseTime steps workers = timeNeeded' (drop 1 order) [] (take 1 order) (take workers [0..]) [] [0..]
  where order = properOrder steps
        prerequisiteMap = getPrerequisiteMap steps
        initWorkersAvailable n = fromList $ map initWorker $ take n [0..]
        initWorker n = (n, (0, Nothing))
        timeNeeded' :: [Char] -> [Char] -> [Char] -> [Int] -> [(Int, Char, Int)] -> [Int] -> Int
        timeNeeded' [] _ [] _ [] (time:_) = time
        timeNeeded' remaining done available idleWorkers busyWorkers (time:moreTime) =
          let (doneWorkers, stillBusyWorkers) = partition isDone busyWorkers
              doneSteps = map (\(_, a, _) -> a) doneWorkers
              isDone (id, a, t) = t < time
              newDone = done ++ doneSteps
              newRemaining = remaining \\ newDone
              nowAvailable = nub $ sort ((findNowAvailable newDone newRemaining prerequisiteMap) ++ (available \\ newDone))
              newIdleWorkers = idleWorkers ++ (map (\(a, _, _) -> a) doneWorkers)
              toQueue = min (length nowAvailable) (length newIdleWorkers)
              stepsToQueue = take toQueue nowAvailable
              queuedWorkers = zip3 (take toQueue newIdleWorkers) stepsToQueue (map ((+ (-1)) . (+time) . (timeForStep baseTime)) stepsToQueue)
              newBusyWorkers = (stillBusyWorkers ++ queuedWorkers)
          in timeNeeded' (newRemaining \\ stepsToQueue) newDone (drop toQueue nowAvailable) (drop toQueue newIdleWorkers) (newBusyWorkers) moreTime

findNowAvailable :: [Char] -> [Char] -> Map Char [Char] -> [Char]
findNowAvailable done remaining prerequisiteMap = intersect remaining $ keys $ Map.filter (isAvailable done) prerequisiteMap
  where isAvailable done prereq = null (prereq \\ done)

solve = do
  steps <- map parseStep <$> lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ properOrder steps
  putStrLn "Part 2:"
  --putStrLn $ show $ timeNeeded 60 steps 5

testInput = [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
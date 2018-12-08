import Data.Char (ord)
import Data.List (sort, nub, (\\), delete, intersect, partition)
import Data.Map.Strict (Map, fromList, keys, (!), elems)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace)

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

timeNeeded baseTime steps workers = timeNeeded' [] [] workers [] [0..]
  where order = properOrder steps
        prerequisiteMap = foldl (\m s -> Map.insert s "" m) (getPrerequisiteMap steps) (noPrerequisite steps)
        timeNeeded' :: [Char] -> [Char] -> Int -> [(Char, Int)] -> [Int] -> Int
        timeNeeded' done available idleWorkers busyWorkers (time:moreTime)
          | length done == length order = time - 1
          | otherwise = 
              let (doneWorkers, stillBusyWorkers) = partition isDone busyWorkers
                  doneSteps = map fst doneWorkers
                  isDone (a, t) = t < time
                  newDone = done ++ doneSteps
                  nowAvailable = sort (((findNowAvailable newDone prerequisiteMap) \\ newDone) \\ (map fst stillBusyWorkers))
                  newIdleWorkers = idleWorkers + (length doneWorkers)
                  toQueue = min (length nowAvailable) newIdleWorkers
                  stepsToQueue = take toQueue nowAvailable
                  queuedWorkers = zip stepsToQueue (map (\s -> time - 1 + timeForStep baseTime s) stepsToQueue)
                  newBusyWorkers = stillBusyWorkers ++ queuedWorkers
              in -- trace ("Time: " ++ (show time) ++ ", done: " ++ newDone ++ ", available: " ++ nowAvailable ++ ", queued: " ++ stepsToQueue ++ ", busy: " ++ (show newBusyWorkers)) $
                 timeNeeded' newDone (nowAvailable \\ stepsToQueue) (newIdleWorkers - toQueue) (newBusyWorkers) moreTime

findNowAvailable :: [Char] -> Map Char [Char] -> [Char]
findNowAvailable done prerequisiteMap = keys $ Map.filter (isAvailable done) prerequisiteMap
  where isAvailable done prereq = null (prereq \\ done)

solve = do
  steps <- map parseStep <$> lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ properOrder steps
  putStrLn "Part 2:"
  putStrLn $ show $ timeNeeded 60 steps 5

testInput = [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
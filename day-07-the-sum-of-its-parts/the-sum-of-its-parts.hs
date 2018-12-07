import Data.Char (ord)
import Data.List (sort, nub, (\\), delete, intersect)
import Data.Map.Strict (Map, fromList, keys, (!), elems)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

--Step L must be finished before step D can begin.
parseStep spec = (spec !! 5, spec !! 36)

noPrerequisite steps = (nub $ map fst steps) \\ (nub $ map snd steps)
allSteps steps = let (a, b) = unzip steps in nub $ a ++ b
getPrerequisites steps key = map fst $ filter ((==key) . snd) steps

getPrerequisiteMap steps = let keys = map snd steps in fromList $ zip keys $ map (getPrerequisites steps) keys

time base stepChar = base + 1 + (ord stepChar - ord 'A')

properOrder steps = determineOrder prerequisiteMap startAt []
  where startAt = noPrerequisite steps
        prerequisiteMap = getPrerequisiteMap steps
        determineOrder _ [] done = reverse done
        determineOrder prereqMap (a:available) done = determineOrder newMap (sort $ newAvailable ++ available) (a:done)
          where removedPrereq = Map.map (delete a) prereqMap
                newAvailable = keys $ Map.filter null removedPrereq
                newMap = Map.filterWithKey (\k _ -> not $ k `elem` newAvailable) removedPrereq

{- Since we're not working efficiently, this is too fast -}
totalTime baseTime steps = (criticalPath ! (head $ reverse order))
  where order = properOrder steps
        criticalPath = foldl updateLongestPathMap Map.empty order
        updateLongestPathMap pathMap next =
          let prerequisites = getPrerequisites steps next
              timeNeeded = (time baseTime next) + (maximum $ [0] ++ map (pathMap !) prerequisites)
          in  Map.insert next timeNeeded pathMap

timeNeeded baseTime steps workers = timeNeeded' (drop 1 order) [] (take 1 order) (initWorkersAvailable workers) [0..]
  where order = properOrder steps
        prerequisiteMap = getPrerequisiteMap steps
        initWorkersAvailable n = fromList $ map initWorker $ take n [0..]
        initWorker n = (n, (0, Nothing))
        timeNeeded' [] _ [] _ (time:_) = time
        timeNeeded' remaining done available workers (time:moreTime) =
          let doneWorkers = Map.filter isDone workers
              doneSteps = elems $ Map.map (fromJust . snd) doneWorkers
              isDone (_, Nothing) = False
              isDone (t, Just a) = t <= time
              newDone = done ++ doneSteps
              nowAvailable = sort ((findNowAvailable newDone remaining prerequisiteMap) ++ available)
              newWorkers = foldl markIdle workers (keys doneWorkers)
              markIdle m k = Map.adjust (const (0, Nothing)) k m
          in if null nowAvailable then timeNeeded' remaining newDone nowAvailable newWorkers moreTime else 0

findNowAvailable :: [Char] -> [Char] -> Map Char [Char] -> [Char]
findNowAvailable done remaining prerequisiteMap = intersect remaining $ keys $ Map.filter (isAvailable done) prerequisiteMap
  where isAvailable done prereq = null (prereq \\ done)

solve = do
  steps <- map parseStep <$> lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ properOrder steps
  putStrLn "Part 2:"
  putStrLn $ show $ totalTime 60 steps

testInput = [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
import Data.List (sort, nub, (\\), delete)
import Data.Map.Strict (fromList, keys)
import qualified Data.Map.Strict as Map
--Step L must be finished before step D can begin.
parseStep spec = (spec !! 5, spec !! 36)

noPrerequisite steps = (nub $ map fst steps) \\ (nub $ map snd steps)
allSteps steps = let (a, b) = unzip steps in nub $ a ++ b
getPrerequisites steps key = map fst $ filter ((==key) . snd) steps

getPrerequisiteMap steps = let keys = map snd steps in fromList $ zip keys $ map (getPrerequisites steps) keys

properOrder steps = determineOrder prerequisiteMap startAt []
  where startAt = noPrerequisite steps
        prerequisiteMap = getPrerequisiteMap steps
        determineOrder _ [] done = reverse done
        determineOrder prereqMap (a:available) done = determineOrder newMap (sort $ newAvailable ++ available) (a:done)
          where removedPrereq = Map.map (delete a) prereqMap
                newAvailable = keys $ Map.filter null removedPrereq
                newMap = Map.filterWithKey (\k _ -> not $ k `elem` newAvailable) removedPrereq
        

solve = do
  steps <- map parseStep <$> lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ properOrder steps

testInput = [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
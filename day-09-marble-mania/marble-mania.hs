import Data.List (sortOn)
import Data.Map.Strict (Map, empty, alter, toList)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

actualPlayers = 412
actualLastMarble = 71646

scoringMarbles n = scoringMarbles' [0, 1] 1 2 [] 2
  where
    scoringMarbles' list currentMarblePosition length winners step
      | n < step = (reverse winners, list)
      | step `rem` 23 == 0 = scoringMarbles' removedList removedPosition (length - 1) (prepend (step, step + removedMarble) winners) (step + 1)
      | otherwise = trace ((show step) ++ "\t@\t" ++ (show nextPosition)) $ scoringMarbles' addedList nextPosition (length + 1) winners (step + 1)
        where
          nextPosition = (((currentMarblePosition + 1) `rem` length) + 1) `rem` (length + 1)
          addedList = (take (nextPosition) list) ++ (step:(drop (nextPosition) list))
          removedPosition = ((currentMarblePosition - 7) + length) `rem` length
          (removedList, removedMarble) = let (prefix, suffix) = (splitAt (removedPosition) list) in (prefix ++ (tail suffix), head suffix)

scores players lastMarble = foldl foldScore empty scores
  where scores = fst $ scoringMarbles lastMarble
        foldScore scoreMap (pos, score) = alter (addScore [score]) (pos `rem` players) scoreMap
        addScore new old = Just $ (fromMaybe [] old) ++ new

part1' players lastMarble = head $ reverse $ sortOn (sum . snd) $ toList $ scores players lastMarble

prepend a list = a `seq` (a:list)

solve = do
  putStrLn "Part 1:"
  {- Abandoned: Not sure how a doubly linked list can be modeled in Haskell; default lists are too slow -}
  --putStrLn $ show $ part1' actualPlayers actualLastMarble


testHighScores = map (sum . snd . (uncurry part1')) [(10, 1618), (13, 7999), (17,1104), (21,6111), (30, 5807)]


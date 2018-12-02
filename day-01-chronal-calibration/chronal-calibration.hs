import Data.List (foldl', scanl', repeat)
import Data.Set (member, insert)
import qualified Data.Set as Set

addFrequencyChange :: Integer -> String -> Integer
addFrequencyChange previous (sign:change) = (if sign == '+' then (+) else (-)) previous $ read change

firstDuplicate :: Ord a => [a] -> a
firstDuplicate xs =
  let
    firstDuplicate' (x:xs) seen
      | x `member` seen = x
      | otherwise = firstDuplicate' xs $ insert x seen
  in firstDuplicate' xs Set.empty

solve = do
  changes <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ foldl' addFrequencyChange 0 changes
  putStrLn "Part 2:"
  putStrLn $ show $ firstDuplicate $ scanl' addFrequencyChange 0 $ cycle changes
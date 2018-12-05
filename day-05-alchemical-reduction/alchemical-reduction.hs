import Data.Char (toUpper)
import Data.List (sortOn)

reduce (polymer, _) = reduce' polymer ([], 0)
  where
    reduce' (a:b:rest) (reduced, reducedLength)
      | a /= b && toUpper a == toUpper b = reduce' rest (reduced, reducedLength)
      | otherwise = reduce' (b:rest) ((a:reduced), reducedLength `seq` reducedLength + 1)
    reduce' remaining (reduced, reducedLength) = (reverse $ remaining ++ reduced, 1 + reducedLength)

fullReact polymer = untilDone $ iterate reduce (polymer, length polymer)
  where untilDone (a:b:rest)
          | snd a == snd b = a
          | otherwise = untilDone (b:rest)

optimalReduction polymer = foldl1 bestReduction $ map reactWithoutUnit ['a'..'z']
  where
    reactWithoutUnit unit = fullReact $ filter (\c -> toUpper c /= toUpper unit) polymer
    bestReduction a b = if snd a < snd b then a else b

solveOld = do
  putStrLn "Part 1:"
  polymer <- readFile "input.txt"
  let fullyReacted = fullReact polymer
  putStrLn $ show $ snd fullyReacted
  putStrLn "Part 2:"
  putStrLn $ show $ optimalReduction polymer

{-
  Better version: a single pass is enough if you reduce from the right.
  Inspired by the advent of code subreddit solutions megathread
-}  

collapseRight :: [Char] -> [Char]
collapseRight polymer = foldr reduceStep "" polymer
  where reduceStep a (b:reduced)
          | a /= b && toUpper a == toUpper b = reduced
          | otherwise = (a:b:reduced)
        reduceStep remaining reduced = remaining : reduced

collapseLeft :: [Char] -> [Char]
collapseLeft polymer = reverse $ foldl reduceStep "" polymer
  where reduceStep (b:reduced) a 
          | a /= b && toUpper a == toUpper b = reduced
          | otherwise = (a:b:reduced)
        reduceStep reduced remaining = remaining : reduced


optimalCollapse polymer = head $ sortOn length $ map collapseWithoutUnit ['a'..'z']
  where collapseWithoutUnit unit = collapseRight $ filter (\c -> toUpper c /= toUpper unit) polymer

solve = do
  polymer <- readFile "input.txt"
  putStrLn "Part 1:"
  let collapsed = collapseRight polymer
  putStrLn $ show $ length $ collapsed
  putStrLn "Part 1 (L):"
  let collapsedLeft = collapseLeft polymer
  putStrLn $ show $ length $ collapsedLeft
  putStrLn "Part 2:"
  putStrLn $ show $ length $ optimalCollapse polymer

  {- It also seems possible to optimize the collapsed result from part 1: -}
  putStrLn "Part 2 (alternate approach):"
  putStrLn $ show $ length $ optimalCollapse collapsed

testPolymer = "dabAcCaCBAcCcaDA"

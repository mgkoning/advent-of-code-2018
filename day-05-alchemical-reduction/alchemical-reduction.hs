import Data.Char (toUpper)

reduce polymer = reduce' polymer []
  where
    reduce' (a:b:rest) reduced
      | a /= b && toUpper a == toUpper b = reduce' rest reduced
      | otherwise = reduce' (b:rest) (a:reduced)
    reduce' remaining reduced = reverse $ remaining ++ reduced

fullReact polymer = untilDone $ iterate reduce polymer
  where untilDone (a:b:rest)
          | length a == length b = a
          | otherwise = untilDone (b:rest)

optimalReduction polymer = foldl1 bestReduction $ map reactWithoutUnit ['a'..'z']
  where
    reactWithoutUnit unit =
      let result = fullReact $ filter (\c -> toUpper c /= toUpper unit) polymer
      in ((result, unit), length result)
    bestReduction a b = if snd a < snd b then a else b

solve = do
  putStrLn "Part 1:"
  polymer <- readFile "input.txt"
  let fullyReacted = fullReact polymer
  putStrLn $ show $ length fullyReacted
  putStrLn "Part 2:"
  putStrLn $ show $ optimalReduction polymer


testPolymer = "dabAcCaCBAcCcaDA"

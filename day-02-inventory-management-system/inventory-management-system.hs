import Data.List (sort, group)

{- Part 1 -}
tupleProduct :: Num a => (a, a) -> a
tupleProduct (a, b) = a * b

tupleSum :: Num a => (a, a) -> (a, a) -> (a, a)
tupleSum (a, b) (c, d) = (a + c, b + d)

dupCount :: Ord a => [a] -> (Integer, Integer)
dupCount = twoThreeCount . (map length) . group . sort
  where
    twoThreeCount xs = (oneIfTrue $ hasLength 2 xs, oneIfTrue $ hasLength 3 xs)
    hasLength n = any ((==) n)
    oneIfTrue v = if v then 1 else 0

checksum :: [String] -> Integer
checksum = tupleProduct . (foldl1 tupleSum) . (map dupCount)

{- Part 2 -}
commonContents :: (Eq a) => ([a], [a]) -> [a]
commonContents (as, bs) = concat $ zipWith (\a b -> if a == b then [a] else []) as bs

pairs :: Eq a => [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

findAnswer :: [String] -> [((String, String), String)]
findAnswer xs = filter offByOne $ zip ps (map commonContents ps)
  where
    ps = pairs xs
    offByOne ((a, _), b) = length a - 1 == length b

solve :: IO ()
solve = do
  boxIDs <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ checksum boxIDs
  putStrLn "Part 2:"
  putStrLn $ snd $ head $ findAnswer boxIDs

{- Test inputs -}
testInput = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
testInput2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
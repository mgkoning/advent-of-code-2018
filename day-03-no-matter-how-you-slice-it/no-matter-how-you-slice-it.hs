import qualified Data.Text as T
import Data.Text (Text, splitOn)
import Data.Set (Set, fromList, member)
import Data.List (intersect)

data Claim = Claim { claimId :: String, left :: Int, top :: Int, width :: Int, height :: Int } deriving (Eq, Show)
bottom c = top c + height c - 1
right c = left c + width c - 1

{- #1 @ 1,3: 4x4 -}
parseLine :: String -> Claim
parseLine line =
  let num:_:coords:size:[] = splitOn (T.pack " ") (T.pack line)
      left:top:[] = splitOn (T.pack ",") coords
      width:height:[] = splitOn (T.pack "x") size
      readFromText x = read $ T.unpack x
  in Claim (T.unpack $ T.drop 1 num) (readFromText left) (readFromText $ T.init top) (readFromText width) (readFromText height)

pairs :: Eq a => [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

allCoords :: Claim -> [(Int, Int)]
allCoords c = [(x, y)| x <- [left c..right c], y <- [top c..bottom c]]

overlap :: (Claim, Claim) -> [(Int, Int)]
overlap (a, b)
  | right b < left a || right a < left b || bottom a < top b || bottom b < top a = []
  | otherwise = intersect (allCoords a) (allCoords b)

solve = do
  claims <- map parseLine <$> lines <$> readFile "input.txt"
  let overlaps = fromList $ concat $ map overlap $ pairs claims
  putStrLn "Part 1:"
  putStrLn $ show $ length overlaps

  putStrLn "Part 2:"
  putStrLn $ claimId $ head $ filter (\c -> not $ any (`member` overlaps) (allCoords c)) claims

{- Test stuff -}
testClaim1 = Claim "test1" 1 3 4 4
testClaim2 = Claim "test2" 3 1 4 4
testClaim3 = Claim "test3" 5 5 2 2

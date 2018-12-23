import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn, subsequences, maximumBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import qualified Data.Set as Set

import Data.List (mapAccumL)
import qualified Data.IntSet as S
import qualified Data.Vector as V

newtype Vector3 = Vector3 (Int, Int, Int) deriving (Show, Eq, Ord)

origin = Vector3 (0, 0, 0)

data Nanobot = Nanobot { id :: Int, pos :: Vector3, radius :: Int} deriving (Show, Eq, Ord)

readNanobot (id, l) =
  let (_, a) = break (=='<') l
      (b, c) = break (=='>') a
      pos = read $ "(" ++ (drop 1 b) ++ ")"
      r = read $ drop 5 c
  in Nanobot id (Vector3 pos) r

readNanobots input = map readNanobot $ zip [0..] $ lines input

inRangeCount nanobots = length $ filter (inRange baddestBot) nanobots
  where baddestBot = head $ reverse $ sortOn radius nanobots
        inRange (Nanobot _ baddestPos radius) (Nanobot _ pos _) = distance baddestPos pos <= radius

inRange (Nanobot _ p1 radius) (Nanobot _ p2 _) = distance p1 p2 <= radius

distance (Vector3 (x, y, z)) (Vector3 (x', y', z')) = sum [abs $ x - x', abs $ y - y', abs $ z - z']

overlaps (Nanobot _ v1 r1) (Nanobot _ v2 r2) = distance v1 v2 <= (r1 + r2)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

{- "Inspired" by a discussion of the problem on the subreddit. I was stuck on this for so many
   hours I sort of gave up.
   From that discussion also: it seems this may not actually give the correct answer for all inputs.
-}
part2 nanobots = minDistance possibleAnswer
  where cliques = getMaximalCliques overlaps nanobots
        maximalClique = maximumBy (comparing length) cliques
        possibleAnswer = maximumBy (comparing minDistance) maximalClique
        minDistance n@(Nanobot _ v r) = distance v origin - r

solve = do
  nanobots <- readNanobots <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ inRangeCount nanobots
  putStrLn "Part 2:"
  print $ part2 nanobots

testNanobots@(n1:n2:n3:n4:n5:n6:[]) = readNanobots $ unlines [
  "pos=<10,12,12>, r=2",
  "pos=<12,14,12>, r=2",
  "pos=<16,12,12>, r=4",
  "pos=<14,14,14>, r=6",
  "pos=<50,50,50>, r=200",
  "pos=<10,10,10>, r=5"]



{- Used this code from a haskell package for MaximalCliques: -}

-- | Given a list of nodes, and a function that determines whether there is an edge between any two nodes, yields a list of maximal 
-- cliques -- sets of nodes such that every node is connected to every other, and such that no other node may be added while maintaining this property.
getMaximalCliques :: (a -> a -> Bool) -> [a] -> [[a]]
getMaximalCliques tolFun xs = map (map (fst . (V.!) lv) . S.toList) $
                              maximalCliques pickpivot (snd . ((V.!) lv)) (S.fromList $ map fst lnodes)
    where lnodes = zip [0..] xs
          lnodes' = map (\(k,n) -> (n,S.fromList $ filter (/=k) $ map fst $ filter (tolFun n . snd) lnodes)) lnodes
          lv = V.fromList lnodes'
          pickpivot p x = head $ S.elems p ++ S.elems x

-- | The Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph.
-- <http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>. Works on nodes represented as 'Int's.
maximalCliques :: (S.IntSet -> S.IntSet -> Int) -- ^ A function that given two 'IntSet's, chooses a member of one as a pivot.
               -> (Int -> S.IntSet)  -- ^ A function that given a node id, yields the set of its neighbors.
               -> S.IntSet -- ^ The set of all nodes in the graph.
               -> [S.IntSet] -- ^ An enumeration of all maximal cliques in the graph.
maximalCliques pickpivot neighborsOf nodeset = go S.empty nodeset S.empty
    where go r p x
              | S.null p && S.null x = [r]
              | otherwise =
                  let pivot = pickpivot p x
                      step' (p',x') v =
                          let nv  = neighborsOf v
                          in ((S.delete v p', S.insert v x'), go (S.insert v r) (S.intersection nv p') (S.intersection nv x'))
                  in concat . snd $ mapAccumL step' (p,x) $ S.elems (p S.\\ neighborsOf pivot)

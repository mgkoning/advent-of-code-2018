import Prelude hiding (length, drop)
import qualified Prelude as P
import Data.Sequence hiding (reverse)

data RecipeState = RecipeState { elf1 :: Int, elf2 :: Int, recipes :: Seq Int } deriving (Show)

startingState = RecipeState 0 1 (fromList [3,7])

valueAt n state
  | n < length currentRecipes = (currentRecipes `index` n, state)
  | otherwise = valueAt n (expand state)
  where currentRecipes = recipes state

takeFrom n len state = takeFrom' n len state []
  where takeFrom' _ 0 state r = (reverse r, state)
        takeFrom' n len state r = let (v, s) = valueAt n state in takeFrom' (n+1) (len-1) s (v:r)

findSequence target state = findSequence' target state 0 0 (fromList [])
  where targetLength = P.length target
        findSequence' target state n from currentSequence
          | (length currentSequence) < targetLength = next `seq` findSequence' target s next from (currentSequence |> atN)
          | nextSequence == target = (from', s)
          | otherwise =  next `seq` from' `seq` findSequence' target s next from' nextSequence 
          where next = n + 1
                from' = from + 1
                (atN, s) = valueAt n state
                nextSequence = (drop 1 currentSequence) |> atN

expand state =
  let RecipeState elf1 elf2 recipes = state
      elf1Recipe = (recipes `index` elf1)
      elf2Recipe = (recipes `index` elf2)
      recipes' = let (x, y) = (elf1Recipe + elf2Recipe) `divMod` 10 in x `seq` y `seq` if x > 0 then recipes |> x |> y else recipes |> y
      elf1' = (elf1 + 1 + elf1Recipe) `mod` (length recipes')
      elf2' = (elf2 + 1 + elf2Recipe) `mod` (length recipes')
  in elf1' `seq` elf2' `seq` RecipeState elf1' elf2' recipes'

{- Pretty slow, as I feared, but it works! -}
{- Stats from GHCi: (136.47 secs, 64,530,807,864 bytes) [ouch] -}
solve = do
  putStrLn "Part 1:"
  let (r, s') = takeFrom 652601 10 startingState
  putStrLn $ concat $ map show r
  putStrLn "Part 2:"
  let (r', s'') = findSequence (fromList [6,5,2,6,0,1]) s'
  putStrLn $ show r'
import Data.Bits
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

solve = do
  {- Used the program from day 19 for analysis and checking generated sequence.
     Removed it from this file afterwards to clean up. -}
  let allTheMagic = iterate blackMagic 0
  putStrLn "Part 1:"
  print $ head $ drop 1 allTheMagic
  putStrLn "Part 2:"
  let (repeat, lastBeforeCycle, lengthTaken) = findDuplicate allTheMagic
  print $ lastBeforeCycle

{- This was really the only name I could give this as I am unsure what happens. -}
blackMagic :: Int -> Int
blackMagic reg4 =
  let reg1 = reg4 .|. 65536
      reg4' = 16031208 -- not needed but for documentation purposes
      moreMagic reg4 reg1 = (((reg4 + (reg1 .&. 255)) .&. 16777215) * 65899) .&. 16777215
  in foldl moreMagic reg4' $ takeWhile (>0) $ iterate (`div` 256) reg1

{- Checks the sequence for the first duplicate and report the duplicate,
   its predecessor and how long it took to start the cycle. -}
findDuplicate :: [Int] -> (Int, Int, Int)
findDuplicate = findDuplicate' IntSet.empty 0
  where findDuplicate' seen len (a:b:rest) =
          if b `IntSet.member` seen
            then (b, a, len)
            else len `seq` findDuplicate' (IntSet.insert a seen) (len+1) (b:rest)

import Data.List (sortOn)
import Data.Map.Strict (Map, empty, alter, toList)
import Data.Maybe (fromMaybe)

actualPlayers = 412
actualLastMarble = 71646

data Zipper a = Zipper [a] a [a] deriving (Show, Eq)

move z 0 = z
move (Zipper prefix focus suffix) n
  | n < 0 && null prefix = let (p:ps) = reverse (focus:suffix) in move (Zipper ps p []) (n + 1)
  | n < 0 = let (p:ps) = prefix in move (Zipper ps p (focus:suffix)) (n + 1)
  | n > 0 && null suffix = let (s:ss) = reverse (focus:prefix) in move (Zipper [] s ss) (n - 1)
  | otherwise = let (s:ss) = suffix in move (Zipper (focus:prefix) s ss) (n - 1)

focusValue (Zipper _ a _) = a

{- insert newbie to the right of the focus -}
insertRight (Zipper prefix focus suffix) newbie = Zipper (focus:prefix) newbie suffix

{- delete focus and shift focus to right -}
deleteRight (Zipper prefix focus (s:suffix)) = Zipper prefix s suffix

listify (Zipper p f s) = reverse (f:p) ++ s

scoringMarbles n = scoringMarbles' (Zipper [] 0 []) [] 1
  where
    scoringMarbles' zipper winners step
      | n < step = (reverse winners, zipper)
      | step `rem` 23 == 0 =
          let movedZipper = move zipper (-7)
              victim = focusValue movedZipper
              score = step + victim
          in score `seq` scoringMarbles' (deleteRight movedZipper) ((step, score):winners) (step + 1)
      | otherwise = scoringMarbles' (insertRight (move zipper 1) step) winners (step + 1)

scores players lastMarble = foldl foldScore empty scores
  where scores = fst $ scoringMarbles lastMarble
        foldScore scoreMap (pos, score) = alter (addScore [score]) (pos `rem` players) scoreMap
        addScore new old = Just $ (fromMaybe [] old) ++ new

highScore players lastMarble = head $ reverse $ sortOn (snd) $ map sumScores $ toList $ scores players lastMarble
  where sumScores (p, scores) = (p, sum scores)

solve = do
  putStrLn "Part 1:"
  let (_, highscore1) = highScore actualPlayers actualLastMarble
  putStrLn $ show $ highscore1
  putStrLn "Part 2:"
  let (_, highscore2) = highScore actualPlayers (100*actualLastMarble)
  putStrLn $ show $ highscore2


testHighScores = map (snd . (uncurry highScore)) [(10, 1618), (13, 7999), (17,1104), (21,6111), (30, 5807)]


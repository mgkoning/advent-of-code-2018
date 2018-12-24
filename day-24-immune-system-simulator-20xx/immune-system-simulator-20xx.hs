import Text.ParserCombinators.Parsec (Parser, sepBy1, sepEndBy, endBy, (<|>), getInput, setInput, option, optional, try, parse)
import Text.ParserCombinators.Parsec.Char (string, spaces)
import Numeric (readSigned, readDec)
import Data.List (sortBy, foldl', nub)
import Control.Applicative (empty)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe, catMaybes, isJust, isNothing, fromJust)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data Group = Group { groupId :: Int, side :: Side, stats :: GroupStats } deriving (Show, Eq)
data Side = ImmuneSystem | Infection deriving (Show, Eq)

data GroupStats = GroupStats {
  units :: Int, hitPoints :: Int, weakImmune :: WeakImmune, attackDamage :: Int,
  damageType :: DamageType, initiative :: Int } deriving (Show, Eq)

data WeakImmune = WeakImmune { weaknesses :: [DamageType], immunities :: [DamageType] } deriving (Show, Eq)
noWeakImmune = WeakImmune [] []

data DamageType = Slashing | Radiation | Fire | Bludgeoning | Cold deriving (Show, Read, Eq, Enum, Bounded)

effectivePower :: Group -> Int
effectivePower g = let s = stats g in (units s) * (attackDamage s)

comparingDown :: Ord a => (b -> a) -> b -> b -> Ordering
comparingDown = flip . comparing

targetSelectionOrder :: Group -> Group -> Ordering
targetSelectionOrder g1 g2 = 
  case comparingDown effectivePower g1 g2 of
    EQ -> comparingDown (initiative . stats) g1 g2
    r -> r

enemyPriority :: (Group, Int) -> (Group, Int) -> Ordering
enemyPriority (t1, d1) (t2, d2) =
  case comparingDown id d1 d2 of
    EQ -> case comparingDown effectivePower t1 t2 of
            EQ -> comparingDown (initiative . stats) t1 t2
            r -> r
    r -> r

highestInitiative :: IntMap Group -> (IntMap.Key, b) -> (IntMap.Key, b) -> Ordering
highestInitiative groups = comparingDown (initiative . stats . (groups IntMap.!) . fst)

fight :: IntMap Group -> IntMap Group
fight allGroups =
  let groupsList = IntMap.elems allGroups
      phase1Order = sortBy targetSelectionOrder groupsList
      (_, targetSelections) = foldl' chooseTarget (allGroups, []) phase1Order
      phase2Order = sortBy (highestInitiative allGroups) targetSelections
      afterMath = foldl' attack allGroups phase2Order
  in afterMath

chooseTarget :: (IntMap Group, [(Int, Int)]) -> Group -> (IntMap Group, [(Int, Int)])
chooseTarget (availableTargets, s) unit =
  let remainingEnemies = filter ((/= side unit) . side) $ IntMap.elems availableTargets
      bestTarget = listToMaybe $ map fst $ sortBy enemyPriority $ filter ((/=0) . snd) $ map (determineDamage unit) remainingEnemies
  in case bestTarget of
    Nothing -> (availableTargets, s)
    Just target -> (IntMap.delete (groupId target) availableTargets, (groupId unit, groupId target):s)

determineDamage :: Group -> Group -> (Group, Int)
determineDamage unit target =
  let unitDamageType = damageType $ stats unit
      unitDamage = effectivePower unit
      targetImmunities = immunities $ weakImmune $ stats target
      targetWeaknesses = weaknesses $ weakImmune $ stats target
  in (,) target (if unitDamageType `elem` targetImmunities then 0
                 else if unitDamageType `elem` targetWeaknesses then 2 * unitDamage
                 else unitDamage)

attack :: IntMap Group -> (IntMap.Key, IntMap.Key) -> IntMap Group
attack groups (attackerId, defenderId) = if eitherDead then groups else groups'
  where maybeAttacker = IntMap.lookup attackerId groups
        maybeDefender = IntMap.lookup defenderId groups
        eitherDead = isNothing maybeAttacker || isNothing maybeDefender
        attacker = fromJust maybeAttacker
        defender = fromJust maybeDefender
        (_, damageTaken) = determineDamage attacker defender
        unitsDefeated = damageTaken `div` (hitPoints $ stats defender)
        remainingUnits = (units $ stats defender) - unitsDefeated
        attackedDefender = let s = stats defender in defender { stats = s { units = remainingUnits } }
        groups' = IntMap.update (const (if remainingUnits <= 0 then Nothing else Just attackedDefender)) (groupId defender) groups

applyBoost :: Int -> IntMap Group -> IntMap Group
applyBoost boost groups = IntMap.map boostImmuneSystem groups
  where boostImmuneSystem g@(Group _ side s@(GroupStats _ _ _ damage _ _)) =
          if side == Infection then g else g { stats = s { attackDamage = damage + boost }}

combat :: Int -> IntMap Group -> Maybe (IntMap Group)
combat boost groups = determineResult $ (iterate fight boostedImmuneSystem)
  where twoSides g = 2 == (length $ remainingSides g)
        determineResult (a:b:rest) = if not $ twoSides b then Just b
                                     else if a == b then Nothing {- DRAW! -}
                                     else determineResult (b:rest)
        boostedImmuneSystem = applyBoost boost groups 

remainingSides :: IntMap Group -> [Side]
remainingSides g = nub $ map side $ IntMap.elems g

increaseBoosts :: Int -> IntMap Group -> [(Int, Maybe (IntMap Group))]
increaseBoosts from g = let boosts = [from..] in zip [from..] $ map (\i -> combat i g) [from..] 

sumRemainingUnits :: IntMap Group -> Int
sumRemainingUnits result = sum $ map (units . stats) $ IntMap.elems result

part2 :: IntMap Group -> Int
part2 groups = sumRemainingUnits $ fromJust boostedAfterMath
  where (i, boostedAfterMath) = head $ dropWhile (not . done) $ increaseBoosts 1 groups
        done (_, Nothing) = False
        done (_, Just g) = remainingSides g == [ImmuneSystem]

solve = do
  groups <- parseConflict <$> readFile "input.txt"
  putStrLn "Part 1:"
  let afterMath = combat 0 groups
  print $ sumRemainingUnits $ fromJust afterMath
  putStrLn "Part 2:"
  print $ part2 groups


testCombat = combat 0 testGroups
testBoost = part2 testGroups
testGroups = parseConflict testInput

{- Parsing stuff, tried out Parsec again -}
parseConflict :: String -> IntMap Group
parseConflict input = case parse parseInput "" input of
  Left e -> error $ "Failed: " ++ (show e)
  Right (immuneSystem, infection) ->
    IntMap.fromList $ map (\g -> (groupId g, g)) $ immuneSystemGroups ++ infectionGroups
      where toGroups side numberFrom = (map (\(id, stats) -> Group id side stats)) . (zip [numberFrom..])
            immuneSystemGroups = toGroups ImmuneSystem 1 immuneSystem
            infectionGroups = toGroups Infection (1 + length immuneSystem) infection

parseInput :: Parser ([GroupStats], [GroupStats])
parseInput = (,) <$  string "Immune System:" <* eol
                 <*> parseGroupStats `endBy` eol 
                 <*  eol
                 <*  string "Infection:" <* eol
                 <*> parseGroupStats `sepEndBy` eol

parseGroupStats :: Parser GroupStats
parseGroupStats = GroupStats <$> parseInt 
                             <*  string " units each with "
                             <*> parseInt
                             <*  string " hit points "
                             <*> option noWeakImmune parseWeakImmune
                             <*  string "with an attack that does "
                             <*> parseInt
                             <*  spaces
                             <*> parseDamageType
                             <*  string " damage at initiative"
                             <*> parseInt

parseWeakImmune :: Parser WeakImmune
parseWeakImmune = try (WeakImmune <$  string "("
                                  <*> parseDamageTypes "weak to "
                                  <*  optional (string "; ")
                                  <*> option [] (parseDamageTypes "immune to ")
                                  <*  string ") ")
                  <|> (flip WeakImmune <$  string "("
                                       <*> parseDamageTypes "immune to "
                                       <*  optional (string "; ")
                                       <*> option [] (parseDamageTypes "weak to ")
                                       <*  string ") ")


parseInt :: Parser Int
parseInt = do s <- getInput
              case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

parseDamageType :: Parser DamageType
parseDamageType = Slashing    <$ string "slashing"
              <|> Radiation   <$ string "radiation"
              <|> Fire        <$ string "fire"
              <|> Bludgeoning <$ string "bludgeoning"
              <|> Cold        <$ string "cold"

parseDamageTypes :: String -> Parser [DamageType]
parseDamageTypes prefix = string prefix *> parseDamageType `sepBy1` (string ", ")

eol = string "\r\n" <|> string "\n"

testInput = unlines $ [
  "Immune System:",
  "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2",
  "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3",
  "",
  "Infection:",
  "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1",
  "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"]
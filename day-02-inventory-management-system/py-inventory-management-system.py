from functools import reduce
from itertools import groupby, combinations

def withInput(function):
  with open("input.txt") as file:
    return function(file)

boxIDs = withInput(lambda file: [line.strip() for line in file])

twos, threes = 0, 0
for boxID in boxIDs:
  letterCounts = [len([i for i in group]) for key, group in groupby(sorted(boxID))]
  if any([x == 2 for x in letterCounts]):
    twos += 1
  if any([x == 3 for x in letterCounts]):
    threes += 1

print("Part 1:")
print(twos * threes)

def findCommon(one, other):
  commonLetters = [letter for i, letter in enumerate(one) if other[i] == letter]
  return "".join(commonLetters)

print("Part 2:")
pairs = combinations(boxIDs, 2)
for one, other in pairs:
  common = findCommon(one, other)
  if len(common) == len(one) - 1:
    print(common)
    break

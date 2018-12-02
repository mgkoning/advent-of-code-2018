from operator import add
from functools import reduce
from itertools import accumulate, cycle

def withInput(function):
  with open("input.txt") as file:
    function(file)

print("Part 1:")
withInput(lambda file: print(reduce(add, (int(line) for line in file), 0)))

def firstDuplicate(additionSequence):
  seen = set()
  for x in accumulate(cycle(additionSequence)):
    if x in seen:
      return x
    else:
      seen.add(x)

print("Part 2:")
withInput(
  lambda file:
    print(firstDuplicate((int(line) for line in file)))
)

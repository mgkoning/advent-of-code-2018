function* map(fn, iterable) {
  for(i of iterable) {
    yield fn(i);
  }
}

function* accumulate(fn, seed, iterable) {
  let acc = seed;
  yield acc;
  for(i of iterable) {
    acc = fn(acc, i);
    yield acc;
  }
}

function reduce(fn, seed, iterable) {
  let acc = seed;
  for (i of iterable) {
    acc = fn(acc, i);
  }
  return acc;
}

function* take(num, iterable) {
  let n = 0;
  for (i of iterable) {
    if (num < n) { break; }
    yield i;
  }
}

function toArray(iterable) {
  let result = [];
  for (i of iterable) {
    result.push(i);
  }
  return result;
}

function* cycle(iterable) {
  while(true) {
    yield* iterable;
  }
}

module.exports = {
  map: map,
  accumulate: accumulate,
  reduce: reduce,
  take: take,
  toArray: toArray,
  cycle: cycle
}
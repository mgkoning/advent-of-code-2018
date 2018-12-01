let fs = require('fs');
let lib = require('../js-shared/lib.js')

let input = fs.readFileSync('input.txt', 'utf8');
let frequencies = lib.toArray(lib.map(l => parseInt(l, 10), input.split('\r\n')));

let add = (a,b) => a + b;

function firstDuplicate(iterable) {
  let seen = new Set();
  for(i of iterable) {
    if (seen.has(i)) {
      return i;
    }
    seen.add(i);
  }
}

console.log('Part 1');
console.log(`${lib.reduce(add, 0, frequencies)}`);
console.log('Part 2');
console.log(`${firstDuplicate(lib.accumulate(add, 0, lib.cycle(frequencies)))}`);
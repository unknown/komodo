const x = { a: 1, b: { c: 2 } };
console.log(x.a + x.b.c);

const sum = function sum(x) {
  return x.a + x.b.c;
};

console.log(sum(x));

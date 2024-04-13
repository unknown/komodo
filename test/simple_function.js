/* a + b */
const sum = function sum(a, b) {
  return a + b;
};

/* a * b */
const product = function product(a, b) {
  return a * b;
};

/* (a + b)^2 */
const sumSquare = function sumSquare(a, b) {
  const square = function square(x) {
    return product(x, x);
  };
  return square(a) + product(2, product(a, b)) + square(b);
};

console.log(sum(1, 2));
console.log(product(1, 2));
console.log(sumSquare(1, 2));

/* a + b */
function sum(a, b) {
  return a + b;
}

/* a * b */
function product(a, b) {
  return a * b;
}

/* (a + b)^2 */
function sumSquare(a, b) {
  function square(x) {
    return product(x, x);
  }
  return square(a) + product(2, product(a, b)) + square(b);
}

console.log(sum(1, 2));
console.log(product(1, 2));
console.log(sumSquare(1, 2));

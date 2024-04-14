let a = 1;
console.log(a);
a = 2;
console.log(a);

let b = function b(x) {
  return x * 1;
};
console.log(b(a));
b = function b(x) {
  return x * 2;
};
console.log(b(a));

const f2 = function f2(a, b) {
  const f3 = function f3(c, d) {
    c = c + 1;
    d = c - 1;
    return a * b + c * d;
  };

  a = a * 2;
  b = b * 2 - a;

  return a * b * f3(a + b, a - b);
};

const f1 = function f1(a, b) {
  return f2(a + 1, b - 1);
};

console.log(f1(1, 2));

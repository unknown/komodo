function f1(a, b) {
  f2(a, b);
}

function f2(a, b) {
  function f3(a, b) {
    a + b;
  }
  f3(a, b);
}

f1(1, 2);

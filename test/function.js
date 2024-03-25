function f1(a, b) {
  a + b;
}

function f2(a, b) {
  function f3(a, b) {
    a + b;
  }
  f3(a, b);
}

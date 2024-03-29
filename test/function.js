function f1(a, b) {
  return f2(a, b) * 8;
}

function f2(a, b) {
  function f3(a, b) {
    return (a + b) * 2;
  }
  return f3(a, b) * 4;
}

const result = f1(1, 2);
console.log(result);

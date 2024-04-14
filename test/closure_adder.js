const counter = function (initial) {
  let count = initial;
  return function (x) {
    count = count + x;
    return count;
  };
};

const increment = counter(0);
console.log(increment(5));
console.log(increment(15));

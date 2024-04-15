const counter = function (initial) {
  let count = initial;
  return function (x) {
    count = count + x;
    return count;
  };
};

const increment = counter(0);
const test = function (increment) {
  console.log(increment(5));
  console.log(increment(10));
};

test(increment);

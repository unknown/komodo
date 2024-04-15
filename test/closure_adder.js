const counter = function counter(initial) {
  let count = initial;
  return function increment(x) {
    count = count + x;
    return count;
  };
};

const increment = counter(0);
console.log(increment(5));
console.log(increment(15));

fn main() {
  let x = 1;
  let y = x + 2;
  let z = if y > 2 {
    1
  } else {
    let a = x + x * x;
    let b = a + 1;
    a + b
  };
  let a = 1;
  let b = 2;
  while z > 0 {
    a = a + 1;
    b = b + 2;
    let c = a + b;
    z = z - c;
  }
  let result = z + y;
}

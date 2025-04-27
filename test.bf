fn main() {
  let x = 1;
  let y = x + 2;
  if x == 1 {
    return;
  }
  y = y + 1;
  let z = y + 3;
  let a = z + 4;
  if a == 8 {
    return;
  } else {
    z = z + 1;
    a = a + 1;
    y = y + 1;
    x = x + 1;
    if x == 2 {
      return;
    }
  }
}

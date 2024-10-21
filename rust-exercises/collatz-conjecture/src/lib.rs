pub fn collatz(mut n: u64) -> Option<u64> {
  if n == 0 {
    return None;
  }

  let mut steps: u64 = 0;

  while n != 1 {
    n = match n % 2 {
      0 => n / 2,
      _ => 3 * n + 1,
    };
    steps += 1;
  }

  Some(steps)
}


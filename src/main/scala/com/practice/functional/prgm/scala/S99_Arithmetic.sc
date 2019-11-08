/**
  P22: GCD
  */

def gcd(m: Int, n: Int): Int = if(n == 0) m else gcd(n, m%n)
gcd(63, 36)
gcd(36,63)


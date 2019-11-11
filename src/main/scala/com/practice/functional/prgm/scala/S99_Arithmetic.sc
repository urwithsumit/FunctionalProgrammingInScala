/**
  * P32 (**) Determine the greatest common divisor of two positive integer numbers.
  * Use Euclid's algorithm.
  * scala> gcd(36, 63)
  * res0: Int = 9
  */


def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
gcd(63, 36)
gcd(36, 63)

class S99Int(m: Int = 1) {
  /**
    * P31 (**) Determine whether a given integer number is prime.
    * scala> 7.isPrime
    * res0: Boolean = true
    *
    * @return
    */
  def isPrime: Boolean = (2 to Math.sqrt(m).toInt).forall(m % _ != 0)

  /**
    * P34 (**) Calculate Euler's totient function phi(m).
    * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that
    * are coprime to m.
    * scala> 10.totient
    * res0: Int = 4
    *
    * @return
    */
  def totient = (1 to m).filter(isCoprime(_)).length

  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    * Two numbers are coprime if their greatest common divisor equals 1.
    * scala> 35.isCoprimeTo(64)
    * res0: Boolean = true
    *
    * @param n
    */
  def isCoprime(n: Int): Boolean = gcd(m, n) == 1

  /**
    * P36 (**) Determine the prime factors of a given positive integer (2).
    * Construct a list containing the prime factors and their multiplicity.
    * scala> 315.primeFactorMultiplicity
    * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
    * Alternately, use a Map for the result.
    *
    * scala> 315.primeFactorMultiplicity
    * res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
    */

  def primeFactorMultiplicity() = primeFactors().foldLeft(Map[Int, Int]())((r, c) => r.get(c) match {
    case Some(x) => r.updated(c, x + 1)
    case _ => r.updated(c, 1)
  })

  /**
    * P35 (**) Determine the prime factors of a given positive integer.
    * Construct a flat list containing the prime factors in ascending order.
    * scala> 315.primeFactors
    * res0: List[Int] = List(3, 3, 5, 7)
    */

  def primeFactors(): List[Int] = {
    def factors(denominator: Int, numerator: Int, acc: List[Int]): List[Int] =

      (denominator * denominator > numerator) match {
        case false if numerator % denominator == 0 => factors(denominator, numerator / denominator, denominator :: acc)
        case false => factors(denominator + 1, numerator, acc)
        case true => numerator :: acc
      }

    factors(2, m, List[Int]())
  }

  /**
    * P39 (*) A list of prime numbers.
    * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
    * scala> listPrimesinRange(7 to 31)
    * res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
    */

  def listPrimeRange(R: Range): List[Int] = {
   val primes =  for {i <- 1 to R.last} yield {
      if(new S99Int(i).isPrime) i else -1
    }

    primes.filter(_ >= R.head).toList

  }


  /**
    * P40 (**) Goldbach's conjecture.
    * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
    * scala> 28.goldbach
    * res0: (Int, Int) = (5,23)
    */

  def goldbach(): (Int, Int) = {
    val list = listPrimeRange(1 to m)

  }

}


new S99Int(1).isPrime
new S99Int(2).isPrime
new S99Int(3).isPrime
new S99Int(5).isPrime
new S99Int(7).isPrime
new S99Int(11).isPrime
new S99Int(13).isPrime
new S99Int(18).isPrime

new S99Int(35).isCoprime(64)
new S99Int(10).totient
new S99Int(315).primeFactors()
new S99Int(315).primeFactorMultiplicity()

new S99Int().listPrimeRange(7 to 31)



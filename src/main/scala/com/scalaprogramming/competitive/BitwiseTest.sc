(-7).toBinaryString
(7).toBinaryString

/**
  * Notes from
  * https://www.hackerearth.com/practice/basic-programming/bit-manipulation/basics-of-bit-manipulation/tutorial/
  *
  */

/**
  * Left Shift ( X >> K ) is equivalent to dividing X by 2^K
  *
  * x >> K equivalent to (X / 2 ^ K)
  **/

// 8 / 2 ^ 1 == 4
8 >> 1
// 8 / 2 ^ 3 == 1
8 >> 3

/**
  * Right Shift is Reverse of Left Shift,
  * instead of dividing it is multiplying x by 2 ^ K
  *
  * X << K equivalent to (X * 2 ^ K)
  **/

// 2 * 2 ^ 1 == 4
2 << 1

// 3 * 2 ^ 3 == 24
3 << 3

/**
  * Operators : ~ bit flipper
  * flips all bits
  *
  */

val _five = ~(5)
_five.toBinaryString

/** *
  * AND ( & ): Bitwise AND is a binary operator that operates on two equal-length bit patterns.
  * If both bits in the compared position of the bit patterns are 1,
  * the bit in the resulting bit pattern is 1, otherwise 0.
  */
(-8).toBinaryString
66.toBinaryString

// 6 and 66 do not have equal bit length.
-8 & 66

//
6 & 8


/**
  * Check if a Number is a Power of 2
  **/
def isPowerOfTwo(x: Int): Boolean = {
  // Properties for numbers which are powers of 2,
  // is that they have one and only one bit set in their binary representation.
  // if x is a power of 2 then x & (x-1) will be 0
  x != 0 && (x & (x - 1)) == 0
}

isPowerOfTwo(8)
isPowerOfTwo(9)
isPowerOfTwo(10)
isPowerOfTwo(6)
isPowerOfTwo(256)
isPowerOfTwo(1024)

/**
  *
  * @param _x
  * @return
  */
def countOnesInBinary(_x: Int): Int = {
  var x = _x
  var count = 0
  while (x != 0) {
    x = (x & (x - 1))
    //    println(x)
    count += 1
  }
  count
}

(-13).toBinaryString.count(_ == '1')

countOnesInBinary(-13)

/**
  * Check if the ith Bit is Set for a Number.
  *
  * @param i
  * @param x
  * @return
  */
def check_ithBitSet(i: Int, x: Int): Boolean = {
  (x & (1 << i)) != 0
}

10.toBinaryString
check_ithBitSet(1, 10)
check_ithBitSet(2, 10)
check_ithBitSet(3, 10)
check_ithBitSet(4, 10)

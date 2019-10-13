import java.io.PrintWriter

import scala.annotation.tailrec

def addNo(x: Int)(y: Int)(z: Int) = x + y + z
val onePlus = addNo(1) _
val limit = 7

println(onePlus(2)(3))
val Result = pascalTriangle(limit, 0, Array.ofDim(limit, limit))

withPrintWriter(new java.io.File("date.txt")) { (x: PrintWriter) =>
  x.println(s"Date: ${new java.util.Date}")
}

println(convertToBinary(183, List()).mkString)

println(convertToBinary(0, List()).mkString)

println(convertToBinary(1, List()).mkString)
println(convertToBinary(2, List()).mkString)
println(convertToBinary(3, List()).mkString)
println(convertToBinary(5, List()).mkString)
assert(convertToBinary(0, List()).mkString == 0.toBinaryString)
assert(convertToBinary(1, List()).mkString == 1.toBinaryString)
assert(convertToBinary(193, List()).mkString == 193.toBinaryString)
assert(convertToBinary(273, List()).mkString == 273.toBinaryString)

def withPrintWriter(file: java.io.File)(op: PrintWriter => Unit) = {
  val Writer = new PrintWriter(file)
  try {
    op.apply(Writer)
  } finally {
    Writer.close()
  }
}

// Convert Decimal to Binary
@tailrec
def convertToBinary(x: Int, acc: List[Int]): List[Int] = {
  x match {
    case 0 if acc.isEmpty => 0 :: acc
    case 0 => acc
    case _ => convertToBinary(x / 2, (x % 2) :: acc)
  }
}

@tailrec
def pascalTriangle(limit: Int, level: Int, acc: Array[Array[Integer]]): Array[Array[Integer]] = {
  level match {
    case 0 => acc(0)(0) = 1
      pascalTriangle(limit, level + 1, acc)
    case _ if level == limit => acc
    case _ => {
      acc(level)(0) = 1
      acc(level)(level) = 1
      for (i <- 1 until level) {
        acc(level)(i) = acc(level - 1)(i - 1) + acc(level - 1)(i)
      }
      pascalTriangle(limit, level + 1, acc)
    }
  }
}

print(Result.map(_.filter(_ > 0).mkString(" ")).mkString("\n"))

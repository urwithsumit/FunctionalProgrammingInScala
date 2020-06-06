for (i <- 1 until 3; j <- "abc") println(s"${i} ${j}")

(1 until 3).flatMap(i => "abc".map(j => println(s"${i} ${j}"))).mkString("")

(1 until 3).foreach(i => "abc".foreach(j => println(s"${i}  $j")))

for (i <- "Sumit") println(i)

//Conditions and Command are call by name, so to evaluate for each iteration.
def WHILE(cond: => Boolean)(command: => Unit): Unit = {
  if (cond) {
    command
    WHILE(cond)(command)
  } else ()
}


def loop1(n: Int) = {
  var x = n
  WHILE(x > 0) {
    x = x - 1
    println("Shoot")
  }
}

loop1(4)

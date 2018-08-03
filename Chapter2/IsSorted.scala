object IsSorted {

  def isSorted[A](as: Array[A],ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length)
        true
      else if (ordered(as(n - 1), as(n)))
        loop(n + 1)
      else
        false
    }

    loop(1)

  }

  def orderedInt(a: Int, b:Int): Boolean =
    if (b >= a) true else false

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2,3,4,5),orderedInt))
    println(isSorted(Array(1,2,3,5,4),orderedInt))
  }
}
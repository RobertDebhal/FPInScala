object Currying {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def main(args: Array[String]): Unit = {
    val func = curry((a: Int, b: Double) => a.toString + b.toString)
    val func2 = func(2)
    println(func2(2.0))
  }

}
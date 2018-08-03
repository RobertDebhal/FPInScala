object Uncurry {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args:Array[String]) = {
    println(uncurry(curry((a: Int, b: Double) => a.toString + b.toString))(1,2.0))
    println(curry((a: Int, b: Double) => a.toString + b.toString)(1)(2.0))
  }
}
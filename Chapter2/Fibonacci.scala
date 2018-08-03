object Fibonacci {
  def fib(n: Int): Int = {
    """
      |Returns the nth Fibonacci number
    """.stripMargin
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n == 0)
        a
      else
        loop(n-1, b, a+b)
    }

    loop(n,0,1)
  }

  def main (args: Array[String]): Unit = {
    println(fib(5))
  }
}
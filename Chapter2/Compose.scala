object Compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args:Array[String]): Unit = {
    println(compose((x:Int) => x+1,(y:String) => y.toInt)("1"))
  }
}
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs) 
  }

  def product(ds: List[Double]): Double = ds match { case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Exercise 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_,t) => t    
    }    
  } 

  //Eercise 3.3
  def setHead[A](l: List[A], r: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,xs) => Cons(r,xs)  
    }  
  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    def loop[A](l: List[A], n: Int): List[A] = {
      if (n==0)
        l
      else
        l match {
          case Nil => Nil
          case Cons(_,t) => drop(t,n-1)
        }
        
    }
    loop(l,n)
  }

  //Exercise 3.5
  def dropWhile[A](l:List[A], f: A => Boolean):List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs,f) else Cons(x,xs)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2)) 
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,Cons(y,Nil)) => Cons(x,Nil) 
      case Cons(z,zs) => Cons(z,init(zs))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f)) 
    }

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((x,y) => 1 + y) 

  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
    }

  def leftSum(ns: List[Int]) = 
    foldLeft(ns, 0)((x,y) => x+y)
  
  def leftProduct(ns: List[Double]) = 
    foldLeft(ns, 1.0)(_ * _)

  def leftLength[A](as: List[A]): Int =
    foldLeft(as,0)((x,y) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as,Nil: List[A])((x,y) => Cons(y,x))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l),z)((a,b) => f(b,a))

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = 
    foldRightViaFoldLeft(a1, a2)((h,l) => Cons(h,l)) 

  def appendList[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l,Nil: List[A])(appendViaFold)

  def addOne(l: List[Int]): List[Int] = 
    //Exercise 3.16
    foldRightViaFoldLeft(l,Nil:List[Int])((l,r) => (Cons(l+1,r)))

  def doubleToString(l: List[Double]): List[String] =
    //Exercise 3.17
    foldRightViaFoldLeft(l,Nil:List[String])((l,r) => Cons(l.toString,r))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    //Exercise 3.18
    foldRightViaFoldLeft(as,Nil:List[B])((l,r) => Cons(f(l),r))

  def localMutationMap[A,B](l: List[A])(f: A => B): List[B] = {
    import collection.mutable.ListBuffer
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t) 
    }
    go(l)
    List(buf.toList: _*)
    // converting from the standard Scala list to the list we've defined here
  }

  //exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as,Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)  

  //exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(as,Nil:List[B])((l,r) => append(f(l),r))

  //exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a => if (f(a)) List(a) else Nil))

  //exercise 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match { 
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ => (sup,sub) match {
        case (Cons(h1,t1),Cons(h2,t2)) =>
          if (h1 == h2) {
            filter(zipWith(t1, t2)((a, b) => a == b))(_ == false) match {
              case Nil => true
              case _ => hasSubsequence(t1, sub)
            }
          }
          else
            hasSubsequence(t1, sub)
        case (_,Nil) => true
        case (Nil,_) => false
      }
    }


}

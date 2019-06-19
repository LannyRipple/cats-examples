import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
import cats.data._       // import Validated, State constructors
import cats.implicits._  // === import cats.instances._; import cats.syntax._

// Think about type of A.  It grows with each recursive call.
def printview[A: Show](in: A)(count: Int): Unit =
  count match {
    case 0 => println(implicitly[Show[A]].show(in))
    case _ => printview((in,in))(count-1)
  }

printview(42)(3)



// Fix
def fix[A,B](f: (A => B) => A => B): A => B = new (A => B) {
  def apply(a: A): B = f(this)(a)
}

def memo_fib(): (Int => Int) => Int => Int = {
  val memo = scala.collection.mutable.Map.empty[Int,Int]

  def fib(f: Int => Int): Int => Int = {
    { n: Int =>
      if (n == 0)
        1
      else {
        val fa = memo.getOrElseUpdate(n-1, f(n-1))
        val fb = memo.getOrElseUpdate(n-2, f(n-2))

        fa + fb
      }
    }
  }

  fib
}

val fib5: Int = fix(memo_fib()).apply(5)
println(s"Fibbonaci(12) = $fib5")

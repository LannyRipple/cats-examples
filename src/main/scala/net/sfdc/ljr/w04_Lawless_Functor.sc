import scala.language.higherKinds
import scala.reflect.ClassTag

trait Functor[F[_]] {
  // ClassTag required to work with Array
  def fmap[A,B: ClassTag](fa: F[A])(f: A => B): F[B]
}

object Instances {

  // Lawful
  implicit object listFunctor extends Functor[List] {
    def fmap[A,B: ClassTag](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  // Lawless
  implicit object arrayFunctor extends Functor[Array] {
    def fmap[A,B: ClassTag](fa: Array[A])(f: A => B): Array[B] =
      fa.map(f)
  }
}


object Noop {

  // identity functor law:  fa.fmap(identity) === fa

  def noop1[F[_], A: ClassTag](fa: F[A])(implicit functor: Functor[F]): F[A] =
    functor.fmap(fa)(identity)

  def noop2[F[_], A: ClassTag](fa: F[A])(implicit functor: Functor[F]): F[A] =
    fa
}

// Run code with `noop1` imported, then using `noop2`.  Keep an eye on
// results between the two runs.
import Noop.{noop1 => noop}

def program_list: List[Int] = {
  import Instances._

  val xs = List(1,2,3)
  var ys = noop(xs)
  ys = ys.updated(0, ys(0) + 1)
  xs
}

def program_array: Array[Int] = {
  import Instances._

  val xs = Array(1,2,3)
  val ys = noop(xs)
  ys(0) += 1
  xs
}

def showArray[A](xs: Array[A]): String = s"Array(${xs.mkString(", ")})"

val results = List(
  s"program_list = $program_list",
  s"program_array = ${showArray(program_array)}"
)

println(results.mkString("\n"))

package net.sfdc.ljr

object p03_HigherKindedTypes {

  /*
   * As programmers we are used to values.
   */

  val x = 3

  /*
   * We are also used to functions.
   */

  def plusOne(x: Int): Int = x + 1

   /*
   * What might be a new insight is that we can view functions as
   * parameterized values.  I.e., I'll give you a value if you give
   * me one first.
   *
   * We can also work with higher-ordered functions.  These are functions
   * that accept another function as a parameter.
   */

  def map[A,B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case h :: rest => f(h) :: map(rest)(f)
    }

  /*
   * For the map above we are calculating a value using a function supplied
   * to the map.  This is pretty much the default view in Scala because higher-
   * ordered functions are added onto a type using OOP.
   *
   * But we can also view higher-ordered function as ones that accept a function
   * and provide some new function.
   */

  def lift[A,B](f: A => B): List[A] => List[B] =
    (as: List[A]) => map(as)(f)

  /*
   *  _     _ _____ __   _ ______  _______
   * |____/    |   | \  | |     \ |______
   * |    \_ __|__ |  \_| |_____/ ______|
   *
   */

  /*
   * Values have types.  To types have something similar?  They do and it's
   * called a "kind" expressed as "*".
   *
   * The kind of Int is *.
   * The kind of a function is * (parameterized value).
   * The kind of List[Int] is *.
   * The kind of Either[String,Int] is *.
   *
   * So the kind of List[Int] is * but what is the kind of List?  A List gives
   * us a new type if we give it a type first.  It can be viewed as a type
   * constructor or a function on kinds.
   *
   * So: The kind of List is * => *.
   * Also: The kind of Either is (*,*) => *.
   *
   * Note that if we can fix the first type for Either, e.g.,
   */

      type RightBiasedEither[A] = Either[String,A]

   /*
   * then RightBiasedEither has kind * => * and we can use it as such in places
   * that expect such a kind.
   *
   * There exists higher-ordered functions which we can express with type
   * (A => B) => A => B.  Here given a function and a value[A] I'll give you
   * a value[B].  But we can also view this as (A => B) => (A => B).  Give me
   * a function and I'll give you a new function.
   */

  def show(x: Int): String = x.toString

  // Hopefully `f` is referentially transparent!
  def doubleShow(f: Int => String)(x: Int): String = s"${f(x)}${f(x)}"

  // Here evaluate `f` only once in case of side-effects
  def fdoubleShow(f: Int => String): Int => String =
    (x: Int) => { val y = f(x); s"$y$y"}

  /*
   * Leading question: Are there higher-kinded types?
   */
  import scala.language.higherKinds

  trait ExampleFunctor[F[_]] {
    def fmap[A,B](fa: F[A])(f: A => B): F[B]
  }

  /*
   * A Functor encapsulates being able to map over a collection of some type.
   * Here I'm using the name `fmap` instead to be clear vs List.map
   *
   * A Functor is a higher-kinded type.  Given a type constructor (kind: * => *)
   * it will return a new type (kind: *) so the kind of a Functor is (* => *) => *.
   */

  implicit object ListFunctor extends ExampleFunctor[List] {

    def fmap[A,B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case Nil => Nil
        case h :: rest => f(h) :: map(rest)(f)
      }
  }

  implicit class ListMapper[A](val xs: List[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit F: ExampleFunctor[List]): List[B] =
      F.fmap(xs)(f)
  }

  List(1,2,3).fmap(plusOne)


  implicit object OptionFunctor extends ExampleFunctor[Option] {

    def fmap[A,B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }
}

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
   * For the map function above we are calculating a value using a function supplied
   * to the map.  This is pretty much the default view of higher-ordered functions in
   * Scala because you tend to call them as methods introduced via OOP.
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
   * Values have types.  Do types have something similar?  They do and it's
   * called a "kind" expressed as "*".
   *
   * The kind of Int is *.
   * The kind of List[Int] is *.
   * The kind of Either[String,Int] is *.
   *
   * So the kind of List[Int] is * but what is the kind of List?  A List gives
   * us a new type if we give it a type first.  It can be viewed as a type
   * constructor or a function on kinds.
   *
   * The kind of List is * -> *.
   * The kind of Option is * -> *.
   * The kind of a Function1 is * -> * -> *.
   * The kind of Map is * -> * -> *.
   * The kind of Either is * -> * -> *.
   *
   * Note that if we can fix the first type for Either, e.g.,
   */

      type RightBiasedEither[A] = Either[String,A]

   /*
    * then RightBiasedEither has kind * => * and we can use it as such in places
    * that expect such a kind.
    */

  /*
   * Higher-kinded types.
   *
   * Like higher-ordered functions that take a function (a paramaterized value)
   * to calculate a value a higher-kinded type takes a type constructor
   * (a paramaterized type) to calculate a type.
   *
   * A Functor is a higher-kinded type (one of many we will discuss).
   *
   * Given a type constructor (kind: * -> *) a Functor will return a new
   * type (kind: *) so the kind of a Functor is (* -> *) -> *.
   */
  import scala.language.higherKinds

  trait Functor[F[_]] {
    def fmap[A,B](fa: F[A])(f: A => B): F[B]
  }

  /*
   * A Functor encapsulates being able to map over a "context".
   *
   * Here are some example contexts.  Why am I saying context instead of data-structure
   * or container?  The first three are obvious containers.  What about the next ones?
   *
   *   Option    - computation that might fail
   *   List      - computation of a non-deterministic value
   *   Either    - computation that might fail and provide a reason of failure
   *
   *   Id        - computation working with pure (referentially transparent) values
   *   Future    - computation which will eventually provide a value
   *   Function1 - calculation of a value using an environment
   *   Cont      - calculation needing a callback to complete (i.e., continuation-passing style)
   */

  import FunctorImplicits._

  // I will use the `fmap` instead of `map` in the implementation to show that
  // we aren't hooking in to the builtin `map` functions.

  Option(3).fmap(plusOne)      // Option(4)

  List(1,2,3).fmap(plusOne)    // List(2,3,4)

  val h = {x: Int => x + 3}.fmap(plusOne)   // Mind blown?
  h(1)  // 5
}

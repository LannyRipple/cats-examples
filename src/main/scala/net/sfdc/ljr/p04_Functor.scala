package net.sfdc.ljr

object p04_Functor {

  /*
   * Functor was talked about a bit earlier.  It is a higher-kinded type constructor
   * encapsulating the idea (and a type signature for implementation) of
   * "things that can be mapped over".  That is, if your data structure can admit
   * a Functor for a type it contains you can change the values (and types)
   * using a `map` helper.
   */

  import scala.language.higherKinds

  trait ExampleFunctor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  /*
   * Laws a Functor must obey to be well behaved.
   *
   *    map(identity) === identity                  // preserve identity
   *    map(f) andThen map(g) === map(f andThen g)  // preserve function composition
   */

  def intToString(x: Int): String = x.toString
  def strToFloat(s: String): Float = s.length.toFloat

  /*
   * Note that this isn't Cats Functor but does show Option.map is well behaved
   */
  Option(3).map(identity)                          // Option(3)
  Option(3).map(intToString).map(strToFloat)       // Option(5.0f)
  Option(3).map(intToString _ andThen strToFloat)  // Option(5.0f)

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  /*
   * Oddly enough Functions (A => B) have a Functor instance.
   * Map does the same thing as `andThen` to compose functions.
   * This needs scalacOption -Ypartial-unification in Scala 2.11
   * (A lot of Cats assumes partial-unification.)
   */

  // h: Int => String
  val h =
    {x: Int => x * 3}                 // Int => Int
      .map {x: Int => x + 100}        // Int => Int
      .map(intToString)               // Int => String

  h(1)  // "103"

  // g: Int => String
  val g =
    {x: Int => x * 3}                 // Int => Int
      .andThen {x: Int => x + 100}    // Int => Int
      .andThen(intToString)           // Int => String

  g(1)  // "103"

  /*
   * Functor itself is more of a building block for more powerful Typeclasses but
   * it has some things to be aware of.  Of note is
   *
   *  __   _ _______ _______ _______ _______ ______
   * | \  | |______ |______    |    |______ |     \
   * |  \_| |______ ______|    |    |______ |_____/
   *
   *  _______ _______  ______ _     _ _______ _______ _     _  ______ _______ _______
   * |______    |    |_____/ |     | |          |    |     | |_____/ |______ |______
   * ______|    |    |    \_ |_____| |_____     |    |_____| |    \_ |______ ______|
   *
   */

  // val list: List[Option[Either[String,Int]]] = ...
  val list = List(None, Option(Left("even prime")), Option(Right(3)), None, Option(Right(5)), None, Option(Right(7)))

  def collatz(n: Int): Int = n % 2 match {
    case 0 => n / 2
    case 1 => 3 * n + 1
  }

  // Tedious to work with
  list.map(_.map(_.right.map(collatz)))

  /*
   * We'll see a lot of discussion around higher-kinded types "composing".
   *
   * Composition means that if F and G are (in this case) Functors then F[G[_]]
   * is also a Functor.  Functors do compose and Cats can help you derive composed
   * Functors.
   *
   * Note here that Either does not have the correct kind as needed by Functor.
   * Either has kind * -> * -> * and Functor needs * -> *.  We can introduce a
   * type alias to fix the first kind to String.  This reshapes the kind to
   * what Functor needs to work.
   */

  type RBEither[A] = Either[String,A]    // Needed so Either will have correct kind
  val ftor = Functor[List].compose[Option].compose[RBEither]

  ftor.map(list)(collatz)

  /*
   * If we let Scala annotate the type for `ftor` it shows
   *
   *    val ftor: Functor[({
   *      type λ[α] = List[Option[RBEither[α]]]
   *      })#λ] = ...
   *
   * This is an inline way of reshaping the needed type.  Compare
   */

  import scala.language.reflectiveCalls
  val ftor2 = Functor[List].compose[Option].compose[({type L[A] = Either[String,A]})#L]

  ftor2.map(list)(collatz)

  /*
   * A puzzler.
   */

  // val ftorSimple = Functor[List].compose[Option].compose[Either]  // Demands implicit for Functor[Either]

  val eth: Either[String,Int] = Right(3)
  eth map intToString                          // Had to find Functor[Either] to work.

  /*
   * SI-2712 (-Ypartial-unification) improved Scala type inference in useful ways
   * to make working with higher-kinded types require less boilerplate.
   *
   * What's going on above is that to declare Either with the correct kind for
   * a Functor we need to use the type alias or inline re-shapes.  When the compiler
   * is left to its own devices to unify types it will try to find a correct shape
   * by trying to fill in types from left to right.  For the map example the compiler
   * saw we were trying to use map on an Either (i.e., needs a Functor so a kind * -> *).
   * It didn't find a map (Functor) for Either[_, _] but did when it tried Either[String,_].
   *
   * To take best advantage of partial-unification when creating types save the final
   * type position for the type you want `map` to work on.
   */
}

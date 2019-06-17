package net.sfdc.ljr

object p04_Functor {

  /*
   * Functor was talked about a bit earlier.  It is a higher-kinded type constructor
   * encapsulating the concept of "things that can be mapped over".  That is to say,
   * if your data structure that can admit a Functor for any type it contains
   * you can change the values (and types) using the Functor as a `map` helper.
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

  // h: Int => Float
  val h =
    {x: Int => x * 3}                 // Int => Int
      .map {x: Int => x + 100}        // Int => Int
      .map(intToString)               // Int => String
      .map(strToFloat)                // String => Float

  h(1)  // 103.0f

  // g: Int => Float
  val g =
    {x: Int => x * 3}                 // Int => Int
      .andThen {x: Int => x + 100}    // Int => Int
      .andThen(intToString)           // Int => String
      .andThen(strToFloat)            // String => Float

  g(1)  // 103.0f

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
   * Functors compose.
   *
   * Composition means that if F and G are (in this case) Functors then F[G[_]]
   * is also a Functor.  Functors compose and Cats can help you easily derive
   * composed Functors.
   *
   * Note here that Either does not have the correct kind as needed by Functor.
   * Either has kind * -> * -> * and Functor needs * -> *.  We can introduce a
   * type alias to fix the first kind to String.  This reshapes the kind to
   * what Functor needs to work.
   */

  type RightBiasedEither[A] = Either[String,A]    // Needed so Either will have correct kind
  val ftor = Functor[List].compose[Option].compose[RightBiasedEither]

  ftor.map(list)(collatz)

  /*
   * If we let Scala annotate the type for `ftor` it shows
   *
   *    val ftor: Functor[({
   *      type λ[α] = List[Option[RightBiasedEither[α]]]
   *      })#λ] = ...
   *
   * This is an inline way of reshaping the needed type.
   */

  import scala.language.reflectiveCalls
  val ftor2 = Functor[List].compose[Option].compose[({type L[A] = Either[String,A]})#L]

  ftor2.map(list)(collatz)

  /*
   * Nested
   *
   * If you are only working with two nested Functors F[G[A]] then Nested can help
   * without having to construct a Functor composition.
   */

  val nested = Nested(List(Option(3), None, Option(7))).map(collatz)
  nested.value

   /*
   * Only being able to work with two Functors at a time is a bit of a limitation
   * for deep nesting but it can prove useful with other things provided by Cats.
   */

  // Here Nested.map is replacing the `.map{_.map{` of List[Option[...]]

  Nested(list).map{_.right.map(collatz)}.value


  /*
   * A puzzler.
   */

  // val ftorSimple = Functor[List].compose[Option].compose[Either]  // Demands implicit for Functor[Either]

  val eth: Either[String,Int] = Right(3)
  eth map intToString                          // How is Either[String,Int] getting `map`?

  /*
   * Answer: Cats adds helpers to Either so acts like other types with Functors
   *
   * Other useful helpers
   */

  eth leftMap {_.length}  // Either[Int,Int]

  eth valueOr {_.length}  // Like Option.getOrElse but uses a function on Left
                          // to provide a value of Right's type.
}

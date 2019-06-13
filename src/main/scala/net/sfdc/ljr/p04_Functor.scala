package net.sfdc.ljr

object p04_Functor {

  /*
   * Functor was talked about a bit earlier.  It is a higher-kinded type constructor
   * encapsulating the idea (and a type signature for implementation) of
   * "things that can be mapped over".  That is, if your data structure can admit
   * a Functor for a type it contains you can change the values (and maybe the
   * types) using a `map` helper.
   */

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
  def strToInt(s: String): Int = s.length

  /*
   * Given two functions f and g, a well behaved Functor preserves composition
   * Note that this isn't Cats Functor but does show Option.map is well behaved
   */
  Option(3).map(intToString).map(strToInt)       // 5
  Option(3).map(intToString _ andThen strToInt)  // 5

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

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
   * Either has kind (*,*) => * and Functor needs * => *.  We can introduce a
   * type alias to fix the first kind to String.  This reshapes the kind to
   * what Functor needs to work.  This pattern happens a lot and SI-2712 improved
   * Scala type inference in useful ways to make working with higher-kinded types
   * require less boilerplate.  The scalaOption to turn it on
   *
   *    "-Ypartial-unification"
   *
   * is not on by default.  Using type aliases provides a workaround for older code.
   */

  type RBEither[A] = Either[String,A]    // Needed so Either will have right kind
  val ftor = Functor[List].compose[Option].compose[RBEither]

  ftor.map(list)(collatz)

  /*
   * If we let Scala annotate the type for `ftor` it shows
   *
   *    val ftor: Functor[({
   *      type λ[α] = List[Option[RBEither[α]]]
   *      })#λ] = ...
   *
   * This is an inline method of reshaping the needed type.  Compare
   */

  import scala.language.reflectiveCalls
  val ftor2 = Functor[List].compose[Option].compose[({type L[A] = Either[String,A]})#L]

  ftor2.map(list)(collatz)
}

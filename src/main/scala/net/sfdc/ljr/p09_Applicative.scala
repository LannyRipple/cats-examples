package net.sfdc.ljr

object p09_Applicative {

  /*
   * An Applicative sits between a Functor and a Monad.  I've played fast and
   * loose with types up to now but the proper type hierchy is
   *
   *    trait Functor[F[_]]
   *       map[A,B](fa: F[A])(f: A => B): F[B]
   *
   *    Applicative[F[_]] extends Functor[F]
   *       pure[A](v: A): F[A]
   *       ap[A,B](f: F[A => B])(fa: F[A]): F[B]       // Note the flip
   *
   *    Monad[F[_]] extends Applicative[F]
   *       flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
   *
   * Applicative lets us apply another A => B function shape and lift it into
   * a context.
   *
   *    Functor:       (A =>   B ) => (F[A] => F[B])
   *    Applicative:  F[A =>   B ] => (F[A] => F[B])
   *    Monad:         (A => F[B]) => (F[A] => F[B])
   *
   * Sadly Scala's syntax and application order really obscures the relationship
   * between pure Functions and Applicatives.  Let's switch to Haskell's syntax
   * for a second.
   *
   * Haskell applies functions with whitespace between the paramaters.  Furthermore
   * a Haskell function only every returns a single result.  Basically EVERY Haskell
   * function is A => B.  It's just that B can itself be a Function or Function
   * returning a Function or so on.
   *
   *    add :: a -> a -> a
   *    add 3 4                     // 7... but
   *    plus3 = add 3               // (\n -> add 3 n)
   *    plus3 4                     // 7
   *
   *    Pure function:    add          3           4       // 7
   *    Applicative:      add <$> Just(3) <*> Just(4)      // Option(7)
   *
   * Here <$> is a function that applies `pure` to the function on the left and
   * then calls `ap` with the value on the right.  The <*> calls `ap` with the
   * value on the right using the Applicative on the left.  So Applicative
   * application is just applying a pure function in a context.
   *
   * Suppose we tried to apply this directly using Scala.
   */

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  def add(x: Int, y: Int): Int = x + y

  (add _).curried  // {x: Int => {y: Int => x + y}}

  // Not very pretty
  Applicative[Option].ap(Applicative[Option].pure((add _).curried))(Some(3)).ap(Some(4))  // Option(7)

  /*
   * Apply
   *
   * Cats has the Apply object which helps with using Applicatives
   * Apply is a weaker form of Applicative without `pure`.  If you need `pure` or
   * Apply doesn't seem to want to work try using Applicative.  They both have
   * `map2` .. `map22`
   *
   * There are some types, e.g., Map[K,?], that don't have an Applicative but do
   * have an Apply which is why there are two such closely related implementations.
   */

  Apply[Option].map2( Some(3), Some(4) )(add)   // Option(7)

  Apply[List].map3( List(1,2), List(2,3,4), List(7) ){_ + _ + _}

  /*
   * Another way which is often convenient to achieve the same result is the
   * extension of Tuples which allows you to use .mapN
   */
  ( List(1,2),
    List(2,3,4),
    List(7) )
    .mapN{ _ + _ + _ }

  /*
   * Applicative is less powerful than Monad.  An Applicative can't perform the
   * flattening that a Monad can which allows you to compose multiple Monadic
   * functions and get a non-nested result.
   *
   * We can implement our add example using a Monad.
   */

  def addem: Option[Int] =
    for {
      x <- Option(3)
      y <- Option(4)
    } yield
      x + y

  /*
   * There are subtle differences in how it is evaluated though.  The Monad version
   * short-circuits and doesn't evaluate any further than finding the first None.
   * An Applicative implementation on the other hand can be made to see can see
   * all the results and collect them up if desired.
   *
   * Cats has a Typeclass, ValidatedNec, designed for Validation tasks.  It provides
   * a Valid(result) or all the Invalid(reason) results in a Chain.
   */

  case class User(fname: String, lname: String)

  type ValidationResult[A] = ValidatedNec[String, A]

  def validateName(name: String, what: String): ValidationResult[String] = {
    if (name.isEmpty)
      s"Empty $what".invalidNec
    else
      name.trim.capitalize.validNec
  }

  val invalidUser =
    ( validateName("", "fName"),
      validateName("", "lName") )
      .mapN(User)

  invalidUser  // Invalid(Chain("Empty fName", "Empty lName"))

  val validUser =
    ( validateName("lanny", "fName"),
      validateName("  ripple", "lName") )
      .mapN(User)

  cats.data.Validated.Valid

  validUser  // Valid(User("Lanny", "Ripple"))
}

package net.sfdc.ljr

object p09_Applicative {

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  /*
   * An Applicative sits between a Functor and a Monad.  I've played fast and
   * loose with types up to now but the proper type hierchy is
   *
   *    trait Functor[F[_]]
   *       map[A,B](fa: F[A])(f: A => B): F[B]
   */

  import scala.language.higherKinds

  trait ExampleApplicative[F[_]] extends Functor[F] {
    def pure[A](v: A): F[A]
    def ap [A,B](f: F[A => B])(fa: F[A]): F[B]     // note `f` and `fa` are flipped
                                                   // relative to Functor and Monad
  }

  /*
   *    Monad[F[_]] extends Applicative[F]
   *       flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
   *
   * (Even this isn't actually what Cats uses but go read the docs to understand why.)
   *
   * Applicative lets us use a pure A => B function shape inside a context.
   *
   *    Functor:       (A  =>   B ) => (F[A] => F[B])
   *    Applicative:  F[A  =>   B ] => (F[A] => F[B])
   *    Monad:         (A  => F[B]) => (F[A] => F[B])
   *
   * If you are wondering about the missing shape.  (We won't discuss these.)
   *
   *    CoMonad:     (F[A] =>   B)  => (F[A] => F[B])
   *
   * Sadly Scala's syntax and application order really obscures the relationship
   * between pure Functions and Applicatives.  Let's switch to Haskell's syntax
   * for a second.
   *
   * Haskell applies functions with whitespace between the paramaters.  Furthermore
   * a Haskell function only every returns a single result.  Basically EVERY Haskell
   * function is A => B.  It's just that B can itself be a Function or Function
   * returning a Function and so on.
   *
   *    add :: Integer -> Integer -> Integer
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
    .mapN{ _ + _ + _ }    // List(10, 11, 12, 11, 12, 13)

  /*
   * Applicative is less powerful than Monad.
   *
   * An Applicative can't perform the flattening that a Monad can which allows
   * you to compose multiple Monadic functions and get a non-nested result
   * (e.g., Option[Option[Int] ==> Option[Int]).
   *
   * This is not to say they aren't powerful.  They basically provide the
   * ability to functionally program inside a context.  Examples of Applicative
   * systems are Applicative Parsers and Applicative Validators.
   *
   * Additionally, because they have fewer constraints you can make Applicatives
   * for more types than you can make Monads for.  If you can build a Monad for
   * a type that's great because you'll get Applicative and Functor "for free".
   * If you can't make a Monad maybe you can make an Applicative.
   */

  /*
   * Cats has a Typeclass, ValidatedNec, designed for Validation tasks.  It provides
   * a Valid(result) or all the Invalid(reason) results in a Chain.
   */

  case class User(fname: String, lname: String)

  object UsingApplicatives {
    type ValidationResult[A] = ValidatedNec[String, A]

    def validateName(name: String, what: String): ValidationResult[String] = {
      if (name.isEmpty)
        s"Empty $what".invalidNec
      else
        name.trim.capitalize.validNec
    }

    val invalidUser =
      (
        validateName("", "fName"),
        validateName("", "lName")
      )
        .mapN(User)

    invalidUser    // Invalid(Chain("Empty fName", "Empty lName"))

    val validUser =
      (
        validateName("lanny", "fName"),
        validateName("  ripple", "lName")
      )
        .mapN(User)


    validUser    // Valid(User("Lanny", "Ripple"))
  }


  /*
   * Below, the same example but using a Monad.  We can still get a valid
   * (Right) User but the Monad short-circuits at the first error (Left)
   * as seen in the result for `invalidUser`.
   */

  object UsingMonads {
    type ValidationResult[A] = Either[List[String], A]

    def validateName(name: String, what: String): ValidationResult[String] = {
      if (name.isEmpty)
        Left(List(s"Empty $what"))
      else
        Right(name.trim.capitalize)
    }

    val invalidUser =
      for {
        fname <- validateName("", "fName")
        lname <- validateName("", "lName")
      } yield
        User(fname, lname)

    invalidUser // Left(List("Empty fName"))

    val validUser =
      for {
        fname <- validateName("Lanny", "fName")
        lname <- validateName("Ripple", "lName")
      } yield
        User(fname, lname)

    validUser // Right(User("Lanny", "Ripple"))
  }

}

package net.sfdc.ljr

class p05_Monad {

  /*
   * You say "pattern" and everyone feels warm and fuzzy.
   * You say "monad" and everyone loses their minds.
   *
   * I hope by now you see where this is going.  We're going to talk about a
   * Monad which is a Typeclass (helper) introducing some sort of programming
   * pattern.  On the surface this pattern is almost trivial.  It's the number
   * of Monads and the uses of them that makes them interesting.
   *
   * A Monad is a higher-kinded type constructor encapsulating the idea of
   * ...
   *
   * First the trivial part.
   * We are writing an OAuth application.  When we submit a user we get
   * back some profile information as Map[Key,String].  We want to turn
   * Profiles into Users.
   */
  type Key = String
  type Profile = Map[Key,String]

  case class User(fname: String, lname: String, email: String)

  val profileLanny: Profile = Map(
    "firstName" -> "Lanny",
    "lastName"  -> "Ripple",
    "email"     -> "lanny.ripple@salesforce.com"
  )

  val profileLinda: Profile = Map(
    "firstName" -> "Linda",
    "lastName" -> "Yancey"
  )

  def profileToUser_usingMap(profile: Profile): Option[Option[Option[User]]] = {
    def get(key: Key): Option[String] = profile.get(key)

    get("firstName").map { fname =>
    get("lastName").map  { lname =>
    get("email").map     { email =>
      User(fname, lname, email)
    }}}
  }

  // Using `map` the Options nest up.  `flatMap` solve this problem.

  def profileToUser(profile: Profile): Option[User] = {
    def get(key: Key): Option[String] = profile.get(key)

    get("firstName").flatMap { fname =>
    get("lastName").flatMap  { lname =>
    get("email").map         { email =>
      User(fname, lname, email)
    }}}
  }

  // So a Monad let us flatten self-nested contexts.  When first learning/using
  // them this is the easiest way to think about them.  "I need to .map this/these
  // Option value(s) but I'd get back a nested Option.  I should use .flatMap"

  // Scala provides for/yield syntactic sugar for Monads

  def profileToUser2(profile: Profile): Option[User] = {
    def get(key: Key): Option[String] = profile.get(key)

    for {
      fname <- get("firstName")
      lname <- get("lastName")
      email <- get("email")
    } yield
      User(fname, lname, email)
  }


  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  import scala.language.higherKinds

  trait ExampleMonad[M[_]] extends Functor[M] {
    def map[A,B](fa: M[A])(f: A => B): M[B]

    /**
      * Lift a "pure" value into a Monad's context.
      * E.g., Option(3)  // The "pure" 3 has been lifted into the context of computations that might not return a value.
      */
    def pure[A](x: A): M[A]

    /**
      * Given a function that will compute a new contextual value from a pure value
      * apply the function to the contents of a contextual value.
      *
      * This is a little easier to see in it's lifted form.
      *
      *    def liftM[A,B](f: A => M[B]): M[A] => M[B]
      *
      * which shows that you can create a function on monadic values if you
      * provide a monadic function (properly termed a Kleisli Arrow).
      */
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

 /*
  * A Monad is a higher-kinded type constructor encapsulating the idea of
  * computations with side-effects.
  *
  * Put another way they define a computation policy.  Working with Option
  * the policy is that if a part of a computation fails you want the entire
  * computation to fail.
  *
  * Working with IO (a monad encapsulating random or mutable results) if a part
  * of your computation deals with IO you want that context to apply to the entire
  * computation (so you don't accidentally mix it into other contexts).
  */

  /** https://youtu.be/7Dr1d9gJb5E?t=53 */
  object ScottEvilPettingZoo {

    import cats.effect.IO

    // Return casualty count
    def `fireThe "Laser"`: IO[Int] = ???

    /* compiler error */
    // def petKittens(kittens: Int, petsPerKitten: Int): Int =
    //   kittens * petsPerKitten + `fireThe "Laser"`  // !! Int + IO[Int]

    def petKillerKittens(kittens: Int, petsPerKitten: Int): IO[Int] =
      `fireThe "Laser"`.map { casualties =>
        kittens * petsPerKitten + casualties
      }
  }
}

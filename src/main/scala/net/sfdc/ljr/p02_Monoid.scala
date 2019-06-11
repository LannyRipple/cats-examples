package net.sfdc.ljr

object p02_Monoid {

  /*
   * A `monoid` (for a type) is a way to "combine things".
   * It has the following definition.
   */

  trait ExampleMonoid[A] {
    def empty: A
    def combine(a: A, b: A): A
  }

  /*
   * Laws a Monoid must obey to be well behaved.
   *
   *    combine(combine(a,b),c) === combine(a,combine(b,c)) // associativity
   *    combine(empty,a) == a                               // left identity
   *    combine(a,empty) == a                               // right identity
   */

  /*
   * A rather obvious monoid on Int
   */
  object IntAdder extends ExampleMonoid[Int] {
    def empty: Int = 0
    def combine(a: Int, b: Int): Int = a + b
  }

  def mysum(xs: Traversable[Int], adder: ExampleMonoid[Int]): Int =
    xs.foldLeft(adder.empty)(adder.combine)

  /*
   * A type can have more than one Monoid
   */
  object IntMult extends ExampleMonoid[Int] {
    def empty: Int = 1
    def combine(a: Int, b: Int): Int = a * b
  }

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  def myAddAnything[A](xs: Traversable[A])(implicit monoid: Monoid[A]): A =
    xs.foldLeft(monoid.empty){_ |+| _}

  /*
   * Note that for Cats a Monoid[A] provides `empty: A` on top of
   * a Semigroup[A]'s `combine(a: A, b: A): A`.
   * The `|+|` operator is found in cats.syntax.semigroup._
   */

  /* None of this is all that interesting because Scala gives us .sum,
   * .product, etc. right?  Nothing to see here?  Move along?
   *
   *  __   _ _______ _______ _______ _______ ______
   * | \  | |______ |______    |    |______ |     \
   * |  \_| |______ ______|    |    |______ |_____/
   *
   *  _______ _______  ______ _     _ _______ _______ _     _  ______ _______ _______
   * |______    |    |_____/ |     | |          |    |     | |_____/ |______ |______
   * ______|    |    |    \_ |_____| |_____     |    |_____| |    \_ |______ ______|
   *
   * Until you start working with nested structures.  And then Monoids shine.
   * Monoids compose.  If you combine elements with a Monoid and those elements
   * contain other Monoids the inner bits will also combine.
   */

  case class Person(name: String, state: String, zip: Int)

  val people =
    List(
      Person("Lanny", "TX", 77055),
      Person("Lanny", "TX", 77840),
      Person("Lanny", "TX", 77092),
      Person("Lanny", "CA", 95135),
      Person("Lanny", "TX", 79902),
      Person("Lanny", "AZ", 85023),
      Person("Lanny", "TX", 77043),
      Person("Linda", "ID", 83209),
      Person("Linda", "TX", 77840),
      Person("Linda", "TX", 77092),
      Person("Linda", "TX", 79416),
      Person("Linda", "TX", 79902),
      Person("Linda", "AZ", 85023),
      Person("Linda", "TX", 77043)
    )

  // Note importing cats.implicits._ does our heavy lifting
  // - vs -
  //    import cats.instances.list._
  //    import cats.instances.map._
  //    import cats.instances.set._
  //    import cats.instances.tuple._
  //    import cats.instances.int._
  //    import cats.instances.string._

  val peopleWithStateWithZips = {
    // Note derived monoid is: Monoid[Map[Person,Map[String,List[Int]]]]

    people.map { p =>
      Map(p -> Map(p.state -> List(p.zip)))
    }
      .reduce {_ |+| _}

    // .reduce because we know list is not empty.
    // `foldLeft(monoid.empty){_ |+| _}` for safety
  }

  val statesWithPeopleZips = {
    // Note derived monoid is: Monoid[Map[String,(Set[String],Set[Int])]

    people.map{ p =>
      Map(p.state -> (Set(p.name), Set(p.zip)))
    }
      .reduce {_ |+| _}
  }
}

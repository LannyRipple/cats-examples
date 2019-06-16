package net.sfdc.ljr

object p01_Typeclasses {

  /*
   * When you hear "typeclass" think "helper".
   *
   * A Typeclass is a trait parameterized by type or types which is used
   * to create implicit instances.  These instances are pulled in by the compiler
   * as helpers for your code.
   *
   * Well now I've done it.  We have to talk about implicits.  An implicit
   * is just a marker for the compiler to know it can provide a helper
   * or take some other action based on a specific type.
   *
   * Implicits are basically used for four things... Nevermind.  Just read
   * a great article about them -
   *
   *    http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html
   *
   * What we need to know about them is we can create a Typeclass (a helper)
   * and get the compiler to provide it for us when we want it.
   */

  trait ExampleShow[A] {
    def show(a: A): String
  }

  case class Datum(x: Int)

  object Datum {

    // Our default Show[Datum]
    implicit val datumShow: ExampleShow[Datum] = new ExampleShow[Datum] {
      def show(in: Datum): String = s"${in.x}: Datum"
    }

    // A Show[Datum] we could use explicitly
    val debugShow: ExampleShow[Datum] = new ExampleShow[Datum] {
      def show(in: Datum): String = s"Debugging! Woo!! datum=${in.x}"
    }
  }

  def compute(d: Datum)(implicit show: ExampleShow[Datum]): Int = {
    show.show(d)
    d.x + 1
  }

  val datum = Datum(3)
  compute(datum)
  compute(datum)(Datum.debugShow)

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  /*
   * PS - Cats can derive many typeclass/helper types - https://github.com/typelevel/kittens
   */

  case class Address(street: String, city: String, state: String)

  object Address {
    implicit val addressShow: Show[Address] = new Show[Address] {
      def show(in: Address): String = s"${in.street}, ${in.city} ${in.state}"
    }
  }

  case class ContactInfo(phone: String, address: Address)

  case class Person(name: String, contact: ContactInfo)

  object Person {
    import cats.derived.semi
    implicit val personShow: Show[Person] = semi.show
  }

  val person =
    Person("Lanny",
      ContactInfo("ext 123",
        Address("14 Maple", "Post", "TX") ))

  // .show comes in via cats.syntax.show._
  person.show // Person(name = "Lanny", ContactInfo(phone = "ext 123", address = "14 Maple, Post TX"))
}

object TypeclassLaws {

  /*
   * Some Typeclasses provide a set of behavior that obey "laws" (constraints
   * on behavior or preconditions) which can be used to reason about code.
   *
   * An example of laws is for Monoid[A].  Given
   *
   * trait Monoid[A] {
   *   def empty: A
   *   def combine(a: A, b: A): A
   * }
   *
   * You must obey the monoid laws
   *
   *   combine(combine(a,b),c) === combine(a,combine(b,c)) // associativity
   *   combine(empty,a) == a                               // left identity
   *   combine(a,empty) == a                               // right identity
   *
   * If you implement a combine that isn't associative you don't have a lawful Monoid.
   * You very well may have something that is useful but there will be corner cases
   * where the behavior is not well behaved and cannot be counted on.
   *
   * What does "can't be counted on" mean?  As an example on List if .foldLeft
   * and .foldRight use a Monoid's `combine` and `empty` they always provide the
   * same answer.  (Or they should.  As an optimization Scala implements
   * `list.foldRight` as `list.reverse.foldLeft`.  This means that in Scala your
   * function also needs to be commutative [ fxy == fyx ] to get the same answers
   * from both methods.  Luckily, mostly nothing to worry about.)
   */

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  val list = List(1,2,3)
  val good = implicitly[Monoid[Int]]  // additive monoid, the cats default on Int

  // aka: list.sum
  list.foldLeft(good.empty)(good.combine)    // 6
  list.foldRight(good.empty)(good.combine)   // 6

  object bad extends Monoid[Int] {
    def empty: Int = 0
    def combine(a: Int, b: Int): Int = a - b  // `-` is NOT associative and has no identity
  }

  list.foldLeft(bad.empty)(bad.combine)    // -6
  list.foldRight(bad.empty)(bad.combine)   // 2
}

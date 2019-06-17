package net.sfdc.ljr

class p07_ReaderWriterState {

  /*
   * The State Monad is closely related to two other Monads, Reader and Writer.
   *
   * type S = <state>
   * type E = <environment>
   * type L = <log>
   *
   * The State implementation wraps  S => (S, A)
   * The Reader implementation wraps E => A
   * The Writer implementation wraps L => (L, A)
   *
   * Note that if we change Reader to be E => (E, A) where we never modify E
   * then they are all using the same mechanics to achieve diffent goals.
   *
   * One other point.  The Id Monad gets tangled up in this.  The definition of
   * Id is
   *
   *    type Id[A] = A
   *
   * and the Id Monad encapsulates calculations without side-effects.  Cats
   * implementation of State, Reader, and Writer are based on Monad Transformers
   * and Id is here just to track that we don't want to apply some other type
   * of Monad.
   */

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._


  /* The Reader Monad encapsulates the context of values calulated using a
   * (read-only) enviroment.
   *
   * The implementation wraps
   *
   *    Environment => A
   */
  type Symtable = Map[String,Int]

  val symTable =
    Map(
      "a" -> 2,
      "b" -> 3,
      "c" -> 7
    )

  def tablename: Reader[Symtable, String] =
    Reader{ symtab => symtab.keys.toList.sorted.map{k => k(0)}.mkString("")}

  def lookup(sym: String): Reader[Symtable,Int] =
    Reader { symtab => symtab.getOrElse(sym, 0) }

  // The following code gets wired together such that each call will
  // be given the (eventually provided) Symtable as its argument
  val reader =
   for {
    name <- tablename
    a <- lookup("a")
    b <- lookup("b")
    c <- lookup("c")
  } yield
     s"$name :: ${(a + b) * c}"

  reader.run(symTable)  // Id("abc :: 42")


  /*
   * The Writer Monad encapsulates the context of a logging system.
   *
   * The implementation wraps
   *
   *   Log => (Log, A)
   */

  // A Chain is like a List but optimized for prefixing AND suffixing.
  // Because Writer appends each log entry to the end, using a List would give
  // O(N^2) behavior

  def gcd(a: Int, b: Int): Writer[Chain[String], Int] = {
    if (b == 0) {
      // if b == 0 we are done.  Log that fact.
      // .tell returns Writer[Log,Unit] so we want to .map the Unit to our answer `a`
      Writer.tell(Chain(s"gcd finished with $a")).map(_ => a)
    }
    else {
      // reduce the problem a little and log what we did
      // Since `gcd` returns a Writer we want to .flatMap (vs .map above)
      val amb = a % b
      Writer.tell(Chain(s"$a mod $b == $amb")).flatMap(_ => gcd(b, amb))
    }
  }

  gcd(12, 16).run // cats.Id[(List[String], Int)] = (List(12 mod 16 = 12, 16 mod 12 = 4, 12 mod 4 = 0, Finished with 4),4)

  /*
   * The punchline to this very long discussion containing a surprising amount of
   * code is that often you want to use Reader, Writer, and State over the same
   * set of computations.
   *
   * Here is a good writeup of using Reader + Writer to track work done in
   * database transactions - https://underscore.io/blog/posts/2014/07/27/readerwriterstate.html
   *
   * Cats provides the ReaderWriterState Monad which fuses these three Monads into one.
   * In the type you provide the environment, log, state, and result types.
   * You can think of the implementation as wrapping functions of type
   *
   *    (E, L, S) => (L, S, A);  note that E can never be modified while L and S can
   */

}

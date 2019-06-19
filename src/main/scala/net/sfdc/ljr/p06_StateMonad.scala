package net.sfdc.ljr

object p06_StateMonad {

  /*
   * The State monad encapsulates the concept (provides the computation policy)
   * of mutable state.
   */

  /*
   * Motivating example:
   *
   * Our system has a default Config that we might update based on
   * runtime configuration information.
   *
   * We also want to track anything that might impact security.
   */
  type Field = String

  case class Config(/* ... */ commonFields: List[Field], nonCommonFields: List[Field] /* , ... */)

  case class SecurityConcern(field: Field)

  val secureFields = Set(
    "password", "auth_token"
  )

  def checkForConcern(field: String): Option[SecurityConcern] =
    if (secureFields(field)) Option(SecurityConcern(field)) else None

  object Defaults {
    val config: Config =
      Config(
        commonFields = List("firstName", "lastName"),
        nonCommonFields = List.empty[Field]
      )
  }

  def overrideFields_I(overrideCommonFields: Seq[Field], overrideNonCommonFields: Seq[Field]): (Config, List[SecurityConcern]) = {
    var config = Defaults.config

    if (overrideCommonFields.nonEmpty)
      config = config.copy(commonFields = overrideCommonFields.toList)

    if (overrideCommonFields.nonEmpty)
      config = config.copy(nonCommonFields = overrideNonCommonFields.toList)

    val concerns =
      (config.commonFields ++ config.nonCommonFields)
        .flatMap(checkForConcern)

    config -> concerns

    // Consider how you would test this thing.
  }


  def overrideFields_II(overrideCommonFields: Seq[Field], overrideNonCommonFields: Seq[Field]): (Config, List[SecurityConcern]) = {
    val config_01 = Defaults.config

    val config_02 =
      if (overrideCommonFields.isEmpty)
        config_01
      else
        config_01.copy(commonFields = overrideCommonFields.toList)

    val config_03 =
      if (overrideCommonFields.isEmpty)
        config_02
      else
        config_02.copy(nonCommonFields = overrideNonCommonFields.toList)

    val concerns =
      (config_03.commonFields ++ config_03.nonCommonFields)
        .flatMap(checkForConcern)

    config_03 -> concerns

    // Still monolithic and tough to test elements
  }

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  /*
   * The implementation of the State monad wraps a function with type
   *
   *    type S      = <type of state>
   *    type A      = <a result>
   *
   *    case class State[S,A](run: S => (S, A))
   *
   * Functor[State[S,_]]
   *    def map[A,B](fa: State[S,A])(f: A => B): State[S,B] =
   *      State { st =>
   *        val (nextSt, a) = fa.run(st)
   *        nextSt -> f(a)
   *      }
   *
   * Monad[State[S,_]]
   *    def pure[S,A](a: A): State[A] =
   *      State { st => st -> a }
   *
   *    def flatMap[S,A](fa: State[S,A])(f: A => State[S,B]): State[S,B] =
   *      State { st =>
   *        val (nextSt, a) = fa.run(st)
   *        val fb = f(a)                           // - returning here: State[S,State[S,B]]
   *        val (nextSt2, b) = fb.run(nextSt)       // - Note correct shape if we just
   *        nextSt2 -> b                            //     f(a).run(nextSt)
   *      }
   */

  def overrideCommonFields(overrides: Seq[Field]): State[Config, List[SecurityConcern]] =
    State { config =>
      val updatedConfig = if (overrides.isEmpty) config else config.copy(commonFields = overrides.toList)
      val concerns = updatedConfig.commonFields.flatMap(checkForConcern)

      updatedConfig -> concerns
    }

  def overrideNonCommonFields(overrides: Seq[Field]): State[Config, List[SecurityConcern]] =
    State { config =>
      val updatedConfig = if (overrides.isEmpty) config else config.copy(nonCommonFields = overrides.toList)
      val concerns = updatedConfig.commonFields.flatMap(checkForConcern)

      updatedConfig -> concerns
    }

  def overrideFields(config: Config, commonOverrides: Seq[Field], nonCommonOverrides: Seq[Field]): (Config, List[SecurityConcern]) = {

    val actions =
      for {
        commonConcerns <- overrideCommonFields(commonOverrides)
        nonCommonConcerns <- overrideNonCommonFields(nonCommonOverrides)
      } yield
        commonConcerns ++ nonCommonConcerns

    // Note: Eval is a Cats Typeclass controlling order of evaluation

    val eval = actions.run(config)  // run  === Eval[(Config, List[SecurityConcern])]
                                    // runS === Eval[Config]
                                    // runA === Eval[List[SecurityConcern]]
    eval.value

    // Consider testing differences to overrideFields_I & _II
  }

  /*
   * As an aside if you just want to modify something (A => A) without needing temporary values
   *
   *   def work(...): Thing = {
   *     val thing_01 = ...
   *     val thing_02 = something(thing_01)
   *     val thing_03 = something_else(thing_02)
   *
   *     something_to_finish(thing_03)
   *   }
   *
   * We can compose the somethings with Function.chain
   */

  // Allow Function.chain to work with vararg of A => A rather than Seq[A => A].
  def chain[A](updates: (A => A)*): A => A = Function.chain(updates)

  def overrideFields_withChain(config: Config, commonOverrides: Seq[Field], nonCommonOverrides: Seq[Field]): Config = {
    val updatedCommon =
      { config: Config => if (commonOverrides.isEmpty) config else config.copy(commonFields = commonOverrides.toList) }

    val updatedNonCommon =
      { config: Config => if (nonCommonOverrides.isEmpty) config else config.copy(commonFields = nonCommonOverrides.toList) }

    // ( updateCommon
    //     andThen updateNonCommon )(config)    // if using just a few updaters
    //

    chain(
      updatedCommon,
      updatedNonCommon
    )(config)
  }

}

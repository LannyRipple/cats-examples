package net.sfdc.ljr

object p06_StateMonad {

  /*
   * The State monad encapsulates the concept (provides the computation policy)
   * of mutable state.
   */

  /*
   * Motivating example.
   *
   * Our system has a default Config that we might update based on runtime information.
   * We also want to track anything that might impact security.
   */
  case class Config(/* ... */ commonFields: List[String], nonCommonFields: List[String] /* , ... */)

  case class SecurityConcern(field: String)

  val secureFields = Set(
    "password", "auth_token"
  )

  def checkForConcern(field: String): Option[SecurityConcern] =
    if (secureFields(field)) Option(SecurityConcern(field)) else None

  object Defaults {
    val config: Config =
      Config(
        commonFields = List("firstName", "lastName"),
        nonCommonFields = List.empty[String]
      )
  }

  def overrideFields_I(overrideCommonFields: Seq[String], overrideNonCommonFields: Seq[String]): (Config, List[SecurityConcern]) = {
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


  def overrideFields_II(overrideCommonFields: Seq[String], overrideNonCommonFields: Seq[String]): (Config, List[SecurityConcern]) = {
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
   *    S => (S, A)
   */

  def overrideCommonFields(overrides: Seq[String]): State[Config, List[SecurityConcern]] =
    State { config =>
      val updatedConfig = if (overrides.isEmpty) config else config.copy(commonFields = overrides.toList)
      val concerns = updatedConfig.commonFields.flatMap(checkForConcern)

      updatedConfig -> concerns
    }

  def overrideNonCommonFields(overrides: Seq[String]): State[Config, List[SecurityConcern]] =
    State { config =>
      val updatedConfig = if (overrides.isEmpty) config else config.copy(nonCommonFields = overrides.toList)
      val concerns = updatedConfig.commonFields.flatMap(checkForConcern)

      updatedConfig -> concerns
    }

  def overrideFields(config: Config, commonOverrides: Seq[String], nonCommonOverrides: Seq[String]): (Config, List[SecurityConcern]) = {

    val actions =
      for {
        commonConcerns <- overrideCommonFields(commonOverrides)
        nonCommonConcerns <- overrideNonCommonFields(nonCommonOverrides)
      } yield
        commonConcerns ++ nonCommonConcerns

    val eval = actions.run(config)  // run  === Eval[(Config, List[SecurityConcern])]
                                    // runS === Eval[Config]
                                    // runA === Eval[List[SecurityConcern]]
    eval.value

    // Consider testing differences to overrideFields_I & _II
  }

  /*
   * As an aside if you just want to modify something without chaining temporary values
   *
   *   def work(...): Thing = {
   *     val thing_01 = ...
   *     val thing_02 = something(thing_01)
   *     val thing_03 = something_else(thing_02)
   *
   *     something_to_finish(thing_03)
   *   }
   *
   * We can compose the somethings with `andThen`
   */

  def chain[A](updates: (A => A)*): A => A = {
    updates.foldLeft(identity[A](_)){(f,g) => f andThen g}
  }

  def overrideFields_withChain(config: Config, commonOverrides: Seq[String], nonCommonOverrides: Seq[String]): Config = {
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

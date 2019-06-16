package net.sfdc.ljr

import scala.concurrent.Future

object p08_MonadTransformers {

  /*
   * Monads, in general, do not compose.  That is, given monads F[_] and G[_],
   * the type F[G[_]] does not always form a Monad.
   *
   * The problem
   * We have available Monad[F[_]] and Monad[G[_]].  We want to write Monad[F[G[_]]]
   * We will need
   *
   *    def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
   *      Monad[F].flatMap(fga){ ga => Monad[G].flatMap(ga){ a => ??? }}
   *
   * For ??? we need to return a G[B] and at the same time the result of the
   * Monad[G].flatMap has to have type F[G[B]] (but by definition can only be G[B]).
   * We can't get there from here.
   *
   * Luckily, while we can't write a general composition of Monads many Monads
   * will compose if we write a custom implementation of composition.  Such
   * structures are called MonadTransformers.
   *
   * It's very difficult to provide short examples of MonadTransformers that provide
   * a good motivation for their use.  This presentation, about dealing with HTTP
   * services, Futures, and Options is one of the best I've come across where
   * MonadTransformers make a real difference.
   *
   * Options in Futures - How to Unsuck Them
   * https://www.youtube.com/watch?v=hGMndafDcc8
   */

  /*
   * Using Cats
   *
   * Well worth a read! -- http://eed3si9n.com/herding-cats/import-guide.html
   */
  import cats._            // import "kernel" definitions.  E.g., trait Monoid[A]
  import cats.data._       // import Validated, State constructors
  import cats.implicits._  // === import cats.instances._; import cats.syntax._

  /*
   * As a quick notes for the presentation the presenter is using the Play web
   * framework.  Play is asynchonous and very much wants you to work with Futures.
   * He also is accessing databases and often comes up with Option results.  To
   * simplify all this he works with the type
   */

  trait Result  // A Result is something Play knows it can serialize for network
                // transport with HTTP.

  // Working with Future needs an execution context.  In production we would provide
  // a custom one and not use the default.
  import scala.concurrent.ExecutionContext.Implicits.global

  type HttpResult[A] = EitherT[Future, Result, A]

  /*
   * EitherT[Future, Result, A] === Future[Either[Result, A]]
   *
   * The idea is that we can build up work in the normal Monadic style (for/yield
   * or chains of .flatMap) but if we hit an error we can provide it as a Left
   * and the error message will be the final result rather than further computation.
   */

  /** Provide helpers to get us into the EitherT[Future,Result,A] shape. */
  object HttpResult {

    def apply[A](v: Future[Either[Result, A]]): HttpResult[A] = EitherT(v)
    def apply[A](v: Either[Result,A]): HttpResult[A] = EitherT(Future.successful(v))
    def fromFuture[A](v: Future[A]): HttpResult[A] = EitherT(v.map(_.asRight[Result]))
    def fromOption[A](oa: Option[A])(err: => Result): HttpResult[A] = apply(oa.toRight(err))
    def fromEither[B,A](eba: Either[B,A])(err: B => Result): HttpResult[A] = apply(eba.leftMap(err))
    def fromFOA[A](foa: Future[Option[A]])(err: => Result): HttpResult[A] = apply(foa.map(_.toRight(err)))

    // any others needed ...
  }

  /** If A is a Result then we can get back a Future[Result] for Play */
  implicit class RichHttpResult[A](val in: HttpResult[Result]) extends AnyVal {
    def runResult: Future[Result] = in.merge
  }

  /** Thrush operator to pretty things up */
  implicit class Thrush[A](val in: A) extends AnyVal {
    def |>[B](f: A => B): B = f(in)
  }

  /** Example computation -- Cheating a little since we don't have needed imports

    def showPosts = Action.async(parse.Json) { request =>

      val result =
        for {
          query <- request.body.validate[Query]  |> HttpResult.fromJsResult
          user <- findUser(query)                |> HttpResult.fromFOA(NotFound("User not found!"))
          posts <- getPosts(user.id)             |> HttpResult.fromFuture
        } yield
          Ok(Json.toJson(posts))

      result.runResult
    }

  */
}

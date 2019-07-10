package net.sfdc.ljr

import scala.concurrent.Future

object p08_MonadTransformers {

  /*
   * Monads, in general, do not compose.  That is, given Monads for F[_] and G[_],
   * there is no generic way to write a Monad for F[G[_]].
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
   * Luckily, while we can't write a general composition of Monads we can often
   * write custom implementations of composition. Such structures are called
   * MonadTransformers.
   */

  /*
   * MonadTransformers are light wrappers that act like the named Monad
   * while being embedded in a Monad of the first type position.
   *
   *    OptionT[F[_], A] === F[Option[A]]
   *
   *    OptionT[Future, A]  === Future[Option[A]]
   *
   * It's very difficult to provide short examples of MonadTransformers that provide
   * a good motivation for their use.  The presentation below, about dealing with HTTP
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
   * He also is accessing databases and often comes up with Option results.  His
   * goal is to write his system in such a way that he can propogate errors
   * ("User not found.  Cannot show blog posts.") but also have a clean
   * workflow.  To achieve the goal he works with the type
   */

  type HttpResult[A] = EitherT[Future, Result, A]

  // Working with Future needs an execution context.  In production we would provide
  // a custom one and not use the default.
  import scala.concurrent.ExecutionContext.Implicits.global

  trait Result  // A Result is something Play knows it can serialize for network
                // transport with HTTP.

  /*
   * EitherT[Future, Result, A] === Future[Either[Result, A]]
   *
   * The idea is that we can build up work in the normal Monadic style (for/yield
   * or chains of .flatMap) but if we hit an error we can provide it as a Left
   * and the error message will be the final result rather than further computation.
   *
   * Where's the Options in all this?  Note that Option[A] is very close in
   * shape to Either[E,A] if we can provide some E for the None case.
   */

  /** Provide helpers to get us into the EitherT[Future,Result,A] shape. */
  object HttpResult {

    def apply[A](fea: Future[Either[Result, A]]): HttpResult[A]            = EitherT(fea)
    def apply[A](ea: Either[Result,A]): HttpResult[A]                      = EitherT(Future.successful(ea))

    // Cannot name `apply` since types would get confused.
    def pure[A](a: A): HttpResult[A]                                       = EitherT(Future.successful(a.asRight[Result]))
    def fromFuture[A](fa: Future[A]): HttpResult[A]                        = EitherT(fa.map(_.asRight[Result]))

    def fromOption[A](oa: Option[A])(err: => Result): HttpResult[A]        = apply(oa.toRight(err))
    def fromFOA[A](foa: Future[Option[A]])(err: => Result): HttpResult[A]  = apply(foa.map(_.toRight(err)))
    def fromEither[B,A](eba: Either[B,A])(err: B => Result): HttpResult[A] = apply(eba.leftMap(err))

    // any others needed ...
  }

  /** If A is a Result then we can get back a Future[Result] for Play */
  implicit class RichHttpResult[A](val in: HttpResult[Result]) extends AnyVal {
    def runResult: Future[Result] = in.merge
  }

  /** Thrush operator to pretty things up */
  implicit class Thrush[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  /** Example computation -- In comment since we don't have needed imports
  {{{

    def validate[A](json: JsRoot): A = ???
    def findUser(query: Query): Option[User] = ???
    def getPosts(id: UserId): Future[List[Post]] = ???


    def showPosts = Action.async(parse.Json) { request =>

      val result =
        for {
          query <- request.body.validate[Query]  |> HttpResult.pure
          user <- findUser(query)                |> HttpResult.fromOption( NotFound("User not found.  Cannot show blog posts.) )
          posts <- getPosts(user.id)             |> HttpResult.fromFuture
        } yield
          Ok(Json.toJson(posts))

      result.runResult
    }

  }}}
  */
}

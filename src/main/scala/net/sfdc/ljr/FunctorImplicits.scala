package net.sfdc.ljr

object FunctorImplicits {

  import p03_HigherKindedTypes.Functor

  implicit object OptionFunctor extends Functor[Option] {

    def fmap[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  implicit object ListFunctor extends Functor[List] {

    def fmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case Nil => Nil
        case h :: rest => f(h) :: fmap(rest)(f)
      }
  }

  import scala.language.reflectiveCalls

  implicit def Function1Functor[R]: Functor[({type L[X] = R => X})#L] =
    new Functor[({type L[X] = R => X})#L] {
      def fmap[A, B](fa: R => A)(f: A => B): R => B =
        fa andThen f
    }

  /* Wrappers to add `fmap` as method */

  import scala.language.higherKinds

  implicit class ListMapper[A](val item: List[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit F: Functor[List]): List[B] =
      F.fmap(item)(f)
  }

  implicit class OptionMapper[A](val item: Option[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit F: Functor[Option]): Option[B] =
      F.fmap(item)(f)
  }

  implicit class Function1Mapper[R, A](val item: R => A) extends AnyVal {
    def fmap[B](f: A => B)(implicit F: Functor[({type L[X] = R => X})#L]): R => B = {
      F.fmap(item)(f)
    }
  }

}

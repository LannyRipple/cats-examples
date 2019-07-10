import cats._
import cats.data._
import cats.implicits._

object syntax {
  object string {
    implicit final class StringWrapper(private val s: String) extends AnyVal {
      def liftNull: String = if (s eq null) "" else s
    }
  }
}

import syntax.string._

case class NameContactPoint(
                           personName: String,
                           salutation: String,
                           firstName: String,
                           middleName: String,
                           lastName: String,
                           secondLastName: String,
                           nameSuffix: String
                           )

// Not the correct implementation but want it to type check.
def clean(in: String): String = in

/*
 * A recent change has extended NameContactPoint with `secondLastName` and other
 * fields.  There was a need to modify normalize() to include `secondLastName`.
 */

// Original implementation
object ProceduralImplementation {

  def normalize(nameContactPoint: NameContactPoint): Option[NameContactPoint] = {
    val namesList = List(nameContactPoint.firstName, nameContactPoint.middleName, nameContactPoint.lastName)
      .map(name => {
        clean(name.liftNull)
      })

    val normalizedNameContactPoint = nameContactPoint.copy(firstName = namesList(0), middleName = namesList(1),
      lastName = namesList(2))

    Option(normalizedNameContactPoint)
  }

}

// Implementation using State
object StateImplementation {

  type St[A] = State[NameContactPoint, A]

  def cleaner(get: NameContactPoint => String, set: (NameContactPoint, String) => NameContactPoint): St[String] =
    State { ncp =>
      val name = get(ncp).liftNull
      val cleanedName = clean(name)
      set(ncp, cleanedName) -> cleanedName
    }

  def normalize(cp: NameContactPoint): Option[NameContactPoint] = {
    val action: St[Unit] =
      for {
        _ <- cleaner(_.firstName, (o,v) => o.copy(firstName = v))
        _ <- cleaner(_.middleName, (o,v) => o.copy(middleName = v))
        _ <- cleaner(_.lastName, (o,v) => o.copy(lastName = v))
        _ <- cleaner(_.secondLastName, (o,v) => o.copy(secondLastName = v))
      } yield
        ()

    action.runS(cp).value.some
  }

}

package com.victorivri.adjective

import com.victorivri.adjective.AdjectiveBase._
import com.victorivri.adjective.AdjectiveMembership._
import org.scalatest.{FreeSpec, Matchers}

class AdjectiveBaseSpec extends FreeSpec with Matchers {

  "Usage example" in {

    // First, we define the precise types that make up our domain/universe/ontology
    object PersonOntology {
      // `Adjective[T]` is the building block of our type algebra
      // Try to make them as atomic as possible
      case object DbId                extends Adjective[Int]    ((id)=> 0 <= id && id < 2000000)
      case object NameSequence        extends Adjective[String] (_.matches("^[A-Z][a-zA-Z]{1,31}$"))
      case object DisallowedSequences extends Adjective[String] (_.toLowerCase.contains("fbomb"))
      case object ScottishLastName    extends Adjective[String] (_ startsWith "Mc")
      case object JewishLastName      extends Adjective[String] (_ endsWith "berg")

      // We use boolean algebra to combine base adjectives into more nuanced adjectives
      val LegalName = NameSequence & ~DisallowedSequences // `~X` negates `X`
      val FirstName = LegalName
      val SomeHeritageLastName = LegalName & (ScottishLastName <+> JewishLastName) // `<+>` stands for Xor, ⊕ is the math notation
    }

    import PersonOntology._
    import TildaFlow._ // so we can use the convenient ~ operator

    // Our Domain is now ready to be used in ADTs, validations and elsewhere.
    // As opposed to monadic types, the preferred way to integrate
    // AdjectiveBase is to use its "successful" type, conveniently accessible through `_.^`
    case class Person (id: DbId.^, firstName: FirstName.^, lastName: SomeHeritageLastName.^)

    // We test membership to an adjective using `mightDescribe`.
    // We string together the inputs, to form an easily-accessible data structure:
    // Either (list of failures, tuple of successes in order of evaluation)
    val validatedInput =
      (DbId                  mightDescribe 123) ~
      (FirstName             mightDescribe "Bilbo") ~
      (SomeHeritageLastName  mightDescribe "McBeggins")

    // The tupled form allows easy application to case classes
    val validPerson = validatedInput map Person.tupled

    // Best way to access is via Either methods or pattern match
    validPerson match {
      case Right(Person(id, firstName, lastName)) => // as you'd expect
      case _ => throw new RuntimeException()
    }

    // we can use `map` to operate on the underlying type without breaking the flow
    validPerson map { _.id map (_ + 1) } shouldBe Right(DbId mightDescribe 124)

    // Trying to precisely type the Includes/Excludes exposes a
    // little bit of clunkiness in the path-dependent types of `val`s
    validPerson shouldBe Right(
      Person(
        Includes(DbId,123), // this works great because DbId is a type, not a `val`
        Includes(FirstName, "Bilbo").asInstanceOf[FirstName.^], // ouch!
        Includes(SomeHeritageLastName, "McBeggins").asInstanceOf[SomeHeritageLastName.^])) // one more ouch.

    // Using the `_.base` we can access the base types if/when we wish
    val baseTypes = validPerson map { person =>
      (person.id.base, person.firstName.base, person.lastName.base)
    }

    baseTypes shouldBe Right((123,"Bilbo","McBeggins"))

    // Using toString gives an intuitive peek at the rule algebra
    //
    // The atomic [Adjective#toString] gets printed out.
    // Beware that both `equals` and `hashCode` are (mostly) delegated to the `toString` implementation
    validPerson.right.get.toString shouldBe
      "Person({ 123 ∈ DbId },{ Bilbo ∈ (NameSequence & ~DisallowedSequences) },{ McBeggins ∈ ((NameSequence & ~DisallowedSequences) & (ScottishLastName ⊕ JewishLastName)) })"

    // Applying an invalid set of inputs accumulates all rules that failed
    val invalid =
      (DbId                  mightDescribe -1) ~
      (FirstName             mightDescribe "Bilbo") ~
      (SomeHeritageLastName  mightDescribe "Ivanov") map Person.tupled

    // We can access the failures to belong to an adjective directly
    invalid shouldBe Left(List(Excludes(DbId,-1), Excludes(SomeHeritageLastName, "Ivanov")))

    // Slightly clunky, but we can translate exclusions to e.g. human-readable validation strings - or anything else
    // TODO Using tuple of exclusions as opposed to a List that disregards types would make it easier.
    val exclusionMappings =
      invalid.left.map { exclusions =>
        exclusions.map {
          case Excludes(DbId, x)                 => s"Bad DB id $x"
          case Excludes(SomeHeritageLastName, x) => s"Bad Last Name $x"
        }
      }

    exclusionMappings shouldBe Left(List("Bad DB id -1", "Bad Last Name Ivanov"))
  }

  "Generate ~ TildaFlow (copy and paste in AdjectiveMembership.TildaFlow)" ignore {

    def genAs (i: Int) = (1 to i) map { n => s"A$n <: AdjectiveBase[N$n]" } mkString ","
    def genNs (i: Int) = (1 to i) map { n => s"N$n" } mkString ","
    def genTup (i: Int) = "(" + ( (1 to i) map { n => s"Includes[A$n,N$n]"} mkString ",") + ")"
    def genABC (i: Int) = (('a' to 'z') take i) mkString ","
    def letter (i: Int) = 'a' + (i-1) toChar

    for (i <- 2 to 21) {
      val j = i + 1
      println (
        s"""
           |implicit class TupExt${i}[${genAs(i)},${genNs(i)}] (v: Either[List[Excludes[_,_]], ${genTup(i)}]) {
           |  def ~ [A$j <: AdjectiveBase[N$j], N$j] (next: AdjectiveMembership[A$j,N$j]): Either[List[Excludes[_,_]], ${genTup(j)}] =
           |    (v, next) match {
           |      case (Right((${genABC(i)})), ${letter(j)}: Includes[A$j,N$j]) => Right((${genABC(j)}))
           |      case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
           |      case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
           |    }
           |}
           |
       """.stripMargin.trim
      )
    }


  }
}

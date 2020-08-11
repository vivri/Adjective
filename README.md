## `Adjective.^`

### Programming is an exercise in linguistics; spice-up Scala types with Adjective

[![Build status](https://circleci.com/gh/vivri/Adjective.svg?style=shield)](https://app.circleci.com/gh/vivri/Adjective)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/Adjective-Scala/community#)

### Sonatype Artifact
__Currently builds for `2.12.x` and `2.13.x`__
```scala
val adjectiveVersion = "0.5.0"

// JVM
libraryDependencies += "com.victorivri" %% "adjective" % adjectiveVersion

// Scala.js
libraryDependencies += "com.victorivri" %%% "adjective" % adjectiveVersion
```

### At a Glance
```scala
import com.victorivri.adjective.AdjectiveBase._

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

// Our Domain is now ready to be used in ADTs, validations and elsewhere.
// As opposed to monadic types, the preferred way to integrate
// Adjective is to use its "successful" type, conveniently accessible through `_.^`
case class Person (id: DbId.^, firstName: FirstName.^, lastName: SomeHeritageLastName.^)
```

### The Problem

The current landscape restricts our ability to express our domain, our __ontology__, in a succinct and intuitive way.

1) We cannot natively apply __adjectives__ to our nouns (e.g. __Positive__ number.)
1) We cannot natively __combine__ our adjectives to form new ones (e.g. Positive __AND__ even number.)
1) We cannot easily maintain semantic information in our types without clunky, non-composable custom wrapper-types.

This prevents us from having native __expressive__ types, such as:

- Natural numbers
- All IPs in a net mask 
- Valid emails
- Obtuse angles
- Dates in the year 2525
- ...

Encoding that domain knowledge into ad-hoc validation methods and smart constructors __strips this information from the domain__, 
often leaving developers confused about valid values, unwritten rules, semantics, and intent. 

And even if we did encode that knowledge into custom classes using smart constructors, we are still missing the ability 
to natively perform __algebra__ on those types, and __derive new types__ from the basic ones.

For example:

- __Router rule range__: NetMask1 __OR__ NetMask2 __AND NOT__ NetMask3
- __Internal email__: Valid email address __AND__ Company hostname __OR__ Subsidiary hostname
- __Valid Names__: Capitalized strings __AND__ Strings of length 2 to 30 __AND__ Strings comprised of only [a-zA-Z]
- ...

### The Solution

__Adjective.^__ solved these problems, such that:

1) You can __create arbitrary restrictions__ on base types (a.k.a. adjectives in linguistics.)
1) You can use __Boolean Algebra__ to arbitrarily __create new adjectives__ from existing ones at runtime.
1) The range of valid values, the semantics and intent __are forever captured__ in the `Adjective`.
1) It is __(somewhat)__ lightweight:
    - Runtime operations are cacheable and predictable (TODO: benchmark).
    - Adjective rules are best stored as singletons to conserve memory footprint and allocation.
    - Minimum boilerplate. 
    - Little knowledge of advanced Scala/Typelevel features required.
    - Zero library dependencies.

### Usage Example

#### The following is a passing spec:

```scala
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
    val exclusionMappings =
      invalid.left.map { exclusions =>
        exclusions.map { y => y match {
            case Excludes(DbId, x)                 => s"Bad DB id $x"
            case Excludes(SomeHeritageLastName, x) => s"Bad Last Name $x"
          }
        }
      }

    exclusionMappings shouldBe Left(List("Bad DB id -1", "Bad Last Name Ivanov"))
  }
```

### Literature Review

1) This document would be incomplete without mentioning the excellent [refined](https://github.com/fthomas/refined)
library. The goals of `refined` are very similar, yet the scope and methods are different. The motivation to create
`Adjective` came in part from `refined`, however `Adjective`'s angle is slightly different, in that it foregoes the ability of compile-time refinement in favor of usability and simplicity.

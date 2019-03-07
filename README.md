## `Adjective.^`

#### Programming is an exercise in linguistics; spice-up Scala types with Adjective

### Maven Central Artifact
```scala
val adjectiveVersion = "0.2"
libraryDependencies += "com.victorivri" %% "adjective" % adjectiveVersion
```

### At a Glance
```scala
    // First, we define the precise types that make up our domain/universe/ontology
    object PersonOntology {
      // `Nuanced[T]` is the building block of our type algebra
      case object DbId              extends Nuanced[Int]    ((id)=> 0 <= id && id < 2000000)
      case object Name              extends Nuanced[String] (_.matches("^[A-Z][a-zA-Z]{1,31}$"))
      case object BadName           extends Nuanced[String] (_.toLowerCase.contains("badword"))
      case object ScottishLastName  extends Nuanced[String] (_ startsWith "Mc")
      case object JewishLastName    extends Nuanced[String] (_ endsWith "berg")

      // We use boolean algebra to combine base rules into more complex rules
      // Note the prefix `~` denotes negation.
      val FirstName = Name & ~BadName
      val LastName  = FirstName & (ScottishLastName | JewishLastName)
    }

    import PersonOntology._
    import Extensions._ // so we can use the convenient ~ operator

    // Our Domain is now ready to be used in ADTs and elsewhere.
    // As opposed to monadic types, the preferred way to integrate
    // Adjective is to use its "successful" type, conveniently accessible
    // through `ThisAdjective.^`
    case class Person (id: DbId.^, firstName: FirstName.^, lastName: LastName.^)
```

### The Problem

We mostly use base-types for lack of a good alternative, since Scala lacks value-dependent types, 
and has an arcane type-algebra.

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
- __Internal email__: Valid email address __AND__ Company email address __OR__ Subsidiary company email address 
- __Valid Names__: Capitalized strings __AND__ Strings of length 2 to 30 __AND__ Strings comprised of only [a-zA-Z]
- ...

#### To sum up:
This restricts our ability to express our domain, our __ontology__, in a succinct way.

1) We cannot natively apply __adjectives__ to our nouns (e.g. __Positive__ number.)
1) We cannot natively __combine__ our adjectives to form new ones (e.g. Positive __AND__ even number.)
1) We cannot easily maintain semantic information in our types without custom wrapper-types.

We should be able to think and express our domain in these terms, but currently, it is very cumbersome, so we mostly end up
using the raw types, and create weak constraints via opaque ad-hoc validations.

### The Solution

__^^[Adjective]__ solved both problems, such that:

1) You can __create arbitrary restrictions__ on base types (a.k.a. __refined__ types, or adjectives in linguistics.)
1) You can use Boolean Algebra to arbitrarily __create new adjectives__ from existing ones.
1) The range of valid values, the semantics and intent __are forever captured__ in the `Adjective`.
1) It is __lightweight__:
    - Runtime operations are cacheable and predictable (TODO: benchmark).
    - Adjective rules are best stored as singletons to conserve memory footprint and allocation.
    - Minimum boilerplate. 
    - Little knowledge of advanced Scala/Typelevel features required.
    - Zero library dependencies.

### Usage Example

#### The following is a passing spec:

```scala

    // First, we define the precise types that make up our domain/universe/ontology
    object PersonOntology {
      // `Nuanced[T]` is the building block of our type algebra
      case object DbId              extends Nuanced[Int]    ((id)=> 0 <= id && id < 2000000)
      case object Name              extends Nuanced[String] (_.matches("^[A-Z][a-zA-Z]{1,31}$"))
      case object BadName           extends Nuanced[String] (_.toLowerCase.contains("badword"))
      case object ScottishLastName  extends Nuanced[String] (_ startsWith "Mc")
      case object JewishLastName    extends Nuanced[String] (_ endsWith "berg")

      // We use boolean algebra to combine base rules into more complex rules
      // Note the prefix `~` denotes negation.
      val FirstName = Name & ~BadName
      val LastName  = FirstName & (ScottishLastName | JewishLastName)
    }

    import PersonOntology._
    import Extensions._ // so we can use the convenient ~ operator

    // Our Domain is now ready to be used in ADTs and elsewhere.
    // As opposed to monadic types, the preferred way to integrate
    // Adjective is to use its "successful" type, conveniently accessible
    // through `ThisAdjective.^`
    case class Person (id: DbId.^, firstName: FirstName.^, lastName: LastName.^)

    // We test membership to an adjective using `mightDescribe`.
    // We string together the inputs, to form an easily-accessible data structure:
    // Either (list of failures, tuple of successes in order of evaluation)
    val validatedInput =
      (DbId      mightDescribe 123) ~
      (FirstName mightDescribe "Bilbo") ~
      (LastName  mightDescribe "McBeggins")

    // The tupled form allows easy application to case classes
    val validPerson = validatedInput map Person.tupled

    validPerson match {
      case Right (Person(id,fname,lname)) =>
        id.base    shouldBe 123
        fname.base shouldBe "Bilbo"
        lname.base shouldBe "McBeggins"

      case Left(_) => throw new RuntimeException()
    }

    // Using the `*` postfix notation, we can access the base types if/when we wish
    val baseTypes = validPerson map { person =>
      (person.id.base, person.firstName.base, person.lastName.base)
    }
    baseTypes shouldBe Right((123,"Bilbo","McBeggins"))

    // Using toString gives an intuitive peek at the rule algebra
    //
    // The atomic [Nuanced#toString] gets printed out.
    // Beware that both `equals` and `hashCode` are (mostly) delegated to the `toString` implementation
    validPerson.right.get.toString shouldBe
      "Person({ 123 ∈ DbId },{ Bilbo ∈ (Name & ~BadName) },{ McBeggins ∈ ((Name & ~BadName) & (ScottishLastName | JewishLastName)) })"

    // Applying an invalid set of inputs accumulates all rules that failed
    val invalid =
      (DbId      mightDescribe -1) ~
      (FirstName mightDescribe "Bilbo") ~
      (LastName  mightDescribe "Ivanov") map Person.tupled

    // We can access the failures to belong to an adjective directly
    invalid shouldBe Left(List(Excludes(DbId,-1), Excludes(LastName, "Ivanov")))

    // Slightly clunky, but we can translate exclusions to e.g. human-readable validation strings
    // Possibly using a tuple of exclusions as opposed to a simple list would make it easier.
    val exclusionMappings =
      invalid.left.map { exclusions =>
        exclusions.map { y => y match {
            case Excludes(DbId, x)     => s"Bad DB id $x"
            case Excludes(LastName, x) => s"Bad Last Name $x"
          }
        }
      }

    exclusionMappings shouldBe Left(List("Bad DB id -1", "Bad Last Name Ivanov"))
```

### Literature Review

1) This document would be incomplete without mentioning the excellent [refined](https://github.com/fthomas/refined)
library. The goals of `refined` are very similar, yet the scope and methods are different. The motivation to create
`Adjective` came in part from `refined`, however `Adjective`'s angle is slightly different, in that it foregoes the ability of compile-time refinement in favor of usability and simplicity.

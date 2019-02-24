# ^^[Adjective]

##### Programming is about communicating intent; increase your expressivity with Adjective.

### The Problem

We mostly use base-types for lack of a good alternative, since Scala lacks value-dependent types, and has a somewhat 
limited type-algebra.

This prevents us from having native __expressive__ types, such as:

- Natural numbers
- All IPs in a net mask 
- Valid emails
- Obtuse angles
- Dates in the year 2525
- ...

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

You should be able to think of your domain in these terms, but currently, it is very cumbersome, so we mostly end up
using the raw types, and create weak constraints via opaque ad-hoc validations.

### The Solution

__^^[Adjective]__ solved both problems, such that:

1) You can __create arbitrary restrictions__ on base types (a.k.a. __refined__ types, or adjectives in linguistics.)
1) You can use Boolean Algebra to arbitrarily __create new adjectives__ from existing ones.
1) It is __lightweight__:
    - Runtime operations are cacheable and predictable.
    - Adjective rules are best stored as singletons.
    - Minimum boilerplate, and little knowledge of advanced Typelevel features.
    - Zero library dependencies.

### Usage Example

#### The following is a passing spec:

```scala
    import net.vivri.adjective.?^._
    import net.vivri.adjective.Adjective._

    // First, we define the precise types that make up our domain/universe/ontology
    object OurOntology {
      // `^^[T] ((t: T) => Boolean)` is the building block of our boolean type algebra
      // read `^^[Int]` as `adjective of int`
      case object DbId              extends ^^[Int]    ((id)=> 0 <= id && id < 2000000)
      case object Name              extends ^^[String] (_.matches("^[A-Z][a-zA-Z]{1,31}$"))
      case object BadName           extends ^^[String] (_.toLowerCase.contains("badword"))
      case object ScottishLastName  extends ^^[String] (_ startsWith "Mc")
      case object JewishLastName    extends ^^[String] (_ endsWith "berg")

      // We use boolean algebra to combine base rules into more complex rules
      // Note the prefix `~` - this denotes negation.
      val FirstNameRule = Name & ~BadName
      val LastNameRule = FirstNameRule & (ScottishLastName | JewishLastName)
    }

    import DSL._ // so we can use the convenient ~ operator
    import OurOntology._

    // Our Domain is now ready to be used in ADTs and elsewhere.
    case class Person (id: DbId.^^, firstName: FirstNameRule.^^, lastName: LastNameRule.^^)

    // We test membership to an adjective using `?^`. It is also the ADT used to store the result.
    // We string together the inputs, to form an easily-accessible data structure:
    // Either (set of failures, tuple of successes in order of evaluation)
    val validatedInput =
      (DbId          ?^ 123) ~
      (FirstNameRule ?^ "Bilbo") ~
      (LastNameRule  ?^ "McBeggins")

    // The tupled form allows easy application to case classes
    val validPerson = validatedInput map Person.tupled

    // Using the `*` postfix notation, we can access the base types if/when we wish
    val baseTypes = validPerson map { person =>
      (person.id*, person.firstName*, person.lastName*)
    }
    baseTypes shouldBe Right((123,"Bilbo","McBeggins"))

    // Using toString gives an intuitive peek at the rule algebra
    //
    // The atomic `!` toString names get printed out - users should feel free to override `toString` for better ,
    // with the caveat that both `equals` and `hashCode` are (mostly) delegated to the `toString` implementation - so
    // make it unique!
    validPerson.right.get.toString shouldBe
      "Person({ 123 ∈ DbId },{ Bilbo ∈ (Name & ~BadName) },{ McBeggins ∈ ((Name & ~BadName) & (ScottishLastName | JewishLastName)) })"

    // Applying an invalid set of inputs accumulates all rules that failed
    val invalid =
      (DbId          ?^ -1) ~
      (FirstNameRule ?^ "Bilbo") ~
      (LastNameRule  ?^ "Ivanov") map Person.tupled

    // We can access the failures to belong to an adjective directly
    invalid shouldBe Left(Set(~^(DbId,-1), ~^(LastNameRule, "Ivanov")))
```

### Literature Review

1) This document would be incomplete without mentioning the excellent [refined](https://github.com/fthomas/refined)
library. The goals of `refined` are very similar, yet the scope and methods are different. The motivation to create
`Adjective` came in part from `refined`, and you should be able to assess the relative similarities, differences, strengths
and weaknesses for yourselves.
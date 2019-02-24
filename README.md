# ^^[Adjective]

##### Programming is an exercise in linguistics; spice-up Scala types with Adjective.

### The Problem

We mostly use base-types for lack of a good alternative, since Scala lacks value-dependent types, and has a somewhat 
limited type-algebra.

This prevents us from having native __expressive__ types, such as:

- Natural numbers
- All IPs in a net mask 
- Valid emails
- Obtuse angles
- Dates in the year 2018
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


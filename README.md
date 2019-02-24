# \><[Narrowing]

#### _Easily create and utilize precise types in your application._

### The Problem

Scala does not have full-fledged value-dependent typing, and a somewhat limited type-algebra.

This prevents us from having native _**precise**_ types, such as:

- Natural numbers
- All IPs in a net mask 
- Valid emails
- Obtuse angles
- Dates in 2018
- ...

We don't have the native ability to encode our domain, our _**ontology**_, onto the type level in a succinct
and expressive way. 

And even if we did encode that knowledge into custom classes using smart constructors, we are still missing the ability 
to natively perform _**algebra**_ on those types, and _**derive new types**_ from the basic ones.

For example:

- __Router rule range__: NetMask1 __OR__ NetMask2 __AND NOT__ NetMask3
- __Internal email__: Valid email address __AND__ Company email address __OR__ Subsidiary company email address 
- __Valid Names__: Capitalized strings __AND__ Strings of length 2 to 30 __AND__ Strings comprised of only [a-zA-Z]
- ...

#### To sum up:
1) We cannot natively apply _**adjectives**_ to our nouns (e.g. _Positive_ number.)
1) We cannot natively _**combine**_ our adjectives to form new ones (e.g. Positive _AND_ even number.)

You should be able to think of your domain in these terms, but currently, it is very cumbersome, so we mostly end up
using the raw types, and create weak constraints via opaque ad-hoc validations.


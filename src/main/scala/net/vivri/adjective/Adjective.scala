package net.vivri.almostfp

import net.vivri.almostfp.?^.{^, ~^}

import scala.util.Try

/**
  * Represents the "contextualization" (i.e. refining) of the base type T.
  *
  * Contains precise types for dependent-type polymorphism in the client code,
  * As well as the boolean algebra operations.
  *
  * See [AdjectiveSpec] `Usage example` for a complete, up-to-date, example on usage.
  */
sealed trait Adjective[T] {
  /**
    * The type of the value
    */
  type V = T

  /**
    * The precise type of this narrowing
    */
  type N <: Adjective[T]

  /**
    * The precise type of success of evaluating this Adjective against a value
    */
  type ^^ = ^[this.N,this.V]

  /**
    * The precise type of failure of evaluating this Adjective against a value
    */
  type ~~ = ~^[this.N,this.V]

  /**
    * Negate/invert the type constraints
    *
    * Can be used without the `.`, e.g.: `~ThisRule`
    */
  def unary_~ : Adjective[T]

  /**
    * Or/Union with another set of type constraints over the same domain
    */
  def | (expr: Adjective[T]): Adjective[T]

  /**
    * And/Intersect with another set of type constraints over the same domain
    */
  def & (expr: Adjective[T]): Adjective[T]

  /**
    * XOR with another set of type constraints over the same domain
    */
  def X (expr: Adjective[T]): Adjective[T]

  /**
    * Validate a value against the constraints.
    *
    * The result should either be `this.^^` if successful, or `this.~~` if failed
    */
  def ?^ (v: T): ?^[N,T]
}

object Adjective {
  type BelongingRule[T] = T => Boolean

  /**
    * Represents a composite expression of type `Adjective`
    */
  trait Expr[T] extends Adjective[T] {
    def unary_~ : Adjective[T] = NOT(this)

    def | (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => OR(this, e)
      case c: Adjective.^^[T] => OR(this, ID(c))
    }

    def & (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => AND(this, e)
      case c: Adjective.^^[T] => AND(this, ID(c))
    }

    def X (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => XOR(this, e)
      case c: Adjective.^^[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[Expr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  /**
    * Represents the atomic building block of contextualizing base type `T`.
    *
    * `^^[T]` should be read as `Adjective of T`
    */
  class ^^[T](val rule: Adjective.BelongingRule[T]) extends Adjective[T] {

    override type N = this.type

    private val id = ID(this)

    override def ?^ (value: T): ?^[N,T] = rule(value) match {
      case true  => ^(this, value)
      case false => ~^(this, value)
    }

    def unary_~ : Adjective[T] = NOT(id)

    def | (b: Adjective[T]): Adjective[T] = b match {
      case e: Adjective.Expr[T] => OR(id, e)
      case c: Adjective.^^[T] => OR(id, ID(c))
    }

    def & (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => AND(id, e)
      case c: Adjective.^^[T] => AND(id, ID(c))
    }

    def X (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => XOR(id, e)
      case c: Adjective.^^[T] => XOR(id, ID(c))
    }

    lazy val pprint = id.pprint
  }

  case class T[Q]() extends Expr[Q] {
    override type N = this.type
    override def ?^(v: Q) = ^(this, v)
    override val pprint: String = "Q"
  }

  case class F[Q]() extends Expr[Q] {
    override type N = this.type
    override def ?^(v: Q) = ~^(this, v)
    override val pprint: String = "F"
  }

  case class ID[T](adjective: Adjective.^^[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) =
      if (adjective rule v) ^(this, v)
      else                  ~^(this, v)

    override val pprint: String = adjective.toString
  }

  case class NOT[T](expr: Expr[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) = {
      expr ?^ v match {
        case ^(_,v)  => ~^(this, v)
        case ~^(_,v) => ^(this, v)
      }
    }

    override val pprint: String = s"~${expr.pprint}"
  }

  case class OR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) =
      (exprA ?^ v, exprB ?^ v) match {
        case (~^(_,_), ~^(_,_)) =>
          ~^(this, v)
        case _ =>
          ^(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} | ${exprB.pprint})"
  }

  case class AND[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) =
      (exprA ?^ v, exprB ?^ v) match {
        case (^(_,_), ^(_,_)) =>
          ^(this, v)
        case _ =>
          ~^(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} & ${exprB.pprint})"
  }

  case class XOR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) = {
      (exprA ?^ v, exprB ?^ v) match {
        case (^(_,_), ^(_,_)) =>
          ~^(this, v)
        case (~^(_,_), ~^(_,_)) =>
          ~^(this, v)
        case _ =>
          ^(this,v)
      }
    }

    override val pprint: String = s"(${exprA.pprint} ⊕ ${exprB.pprint})"
  }
}

/**
  * Represents the result of an application of a [Adjective] algebraic type expression to a value.
  */
sealed trait ?^[N <: Adjective[T], T] {
  val adjective : N
  val * : T
  val pprint: String

  lazy val lift : Either[~^[N,T], ^[N,T]] = this match {
    case x: ^[N,T] => Right(x)
    case x: ~^[N,T] => Left(x)
  }

  def ~ [N2 <: Adjective[T2], T2] (next: ?^[N2,T2]): Either[Set[~^[_,_]],(^[N,T], ^[N2,T2])] =
    (this, next) match {
      case (a: ^[N, T], b: ^[N2, T2]) => Right((a, b))
      case (a,b) => Left(?^.nonMembershipAsSet(a) ++ ?^.nonMembershipAsSet(b))
    }

  override def toString: String = pprint

  override def equals(o: scala.Any): Boolean =
    Try { o.asInstanceOf[?^[N,T]].pprint == pprint } getOrElse false

  override def hashCode(): Int = pprint.hashCode
}

object ?^ {

  def apply[N <: Adjective[T], T](adjective: N, value: T): ?^[adjective.N, T] = adjective ?^ value

  def unapply[N <: Adjective[T], T](arg: ?^[N,T]): Option[(N,T)] = Some((arg.adjective,arg.*))

  /**
    * Represents membership of the value `*` in adjective `N`
    */
  case class ^[N <: Adjective[T], T] private[?^] (adjective : N, * : T) extends ?^[N, T] {
    override val pprint: String = s"{ ${*} ∈ ${adjective.toString} }"
  }

  /**
    * Represents non-membership of the value `*` in adjective `N`
    */
  case class ~^[N <: Adjective[T], T] private[?^] (adjective : N, * : T) extends ?^[N, T] {
    override val pprint: String = s"{ ${*} ∉ ${adjective.toString} }"
  }

  private def nonMembershipAsSet[N <: Adjective[T], T] (x: ?^[N,T]): Set[~^[_,_]] = x match {
    case f: ~^[N,T] => Set(f)
    case _          => Set.empty
  }

  object DSL {
    // Generated from AdjectiveSpec

    implicit class Tup2[A1 <: Adjective[N1],A2 <: Adjective[N2],N1,N2] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2])]) {
      def ~ [A3 <: Adjective[N3], N3] (next: ?^[A3,N3]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3])] =
        (v, next) match {
          case (Right((a,b)), c: ^[A3,N3]) => Right((a,b,c))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup3[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],N1,N2,N3] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3])]) {
      def ~ [A4 <: Adjective[N4], N4] (next: ?^[A4,N4]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4])] =
        (v, next) match {
          case (Right((a,b,c)), d: ^[A4,N4]) => Right((a,b,c,d))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup4[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],N1,N2,N3,N4] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4])]) {
      def ~ [A5 <: Adjective[N5], N5] (next: ?^[A5,N5]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5])] =
        (v, next) match {
          case (Right((a,b,c,d)), e: ^[A5,N5]) => Right((a,b,c,d,e))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup5[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],N1,N2,N3,N4,N5] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5])]) {
      def ~ [A6 <: Adjective[N6], N6] (next: ?^[A6,N6]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6])] =
        (v, next) match {
          case (Right((a,b,c,d,e)), f: ^[A6,N6]) => Right((a,b,c,d,e,f))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup6[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],N1,N2,N3,N4,N5,N6] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6])]) {
      def ~ [A7 <: Adjective[N7], N7] (next: ?^[A7,N7]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f)), g: ^[A7,N7]) => Right((a,b,c,d,e,f,g))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup7[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],N1,N2,N3,N4,N5,N6,N7] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7])]) {
      def ~ [A8 <: Adjective[N8], N8] (next: ?^[A8,N8]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g)), h: ^[A8,N8]) => Right((a,b,c,d,e,f,g,h))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup8[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],N1,N2,N3,N4,N5,N6,N7,N8] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8])]) {
      def ~ [A9 <: Adjective[N9], N9] (next: ?^[A9,N9]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h)), i: ^[A9,N9]) => Right((a,b,c,d,e,f,g,h,i))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup9[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],N1,N2,N3,N4,N5,N6,N7,N8,N9] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9])]) {
      def ~ [A10 <: Adjective[N10], N10] (next: ?^[A10,N10]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i)), j: ^[A10,N10]) => Right((a,b,c,d,e,f,g,h,i,j))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup10[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10])]) {
      def ~ [A11 <: Adjective[N11], N11] (next: ?^[A11,N11]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j)), k: ^[A11,N11]) => Right((a,b,c,d,e,f,g,h,i,j,k))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup11[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11])]) {
      def ~ [A12 <: Adjective[N12], N12] (next: ?^[A12,N12]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k)), l: ^[A12,N12]) => Right((a,b,c,d,e,f,g,h,i,j,k,l))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup12[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12])]) {
      def ~ [A13 <: Adjective[N13], N13] (next: ?^[A13,N13]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l)), m: ^[A13,N13]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup13[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13])]) {
      def ~ [A14 <: Adjective[N14], N14] (next: ?^[A14,N14]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m)), n: ^[A14,N14]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup14[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14])]) {
      def ~ [A15 <: Adjective[N15], N15] (next: ?^[A15,N15]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n)), o: ^[A15,N15]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup15[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15])]) {
      def ~ [A16 <: Adjective[N16], N16] (next: ?^[A16,N16]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)), p: ^[A16,N16]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup16[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16])]) {
      def ~ [A17 <: Adjective[N17], N17] (next: ?^[A17,N17]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)), q: ^[A17,N17]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup17[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17])]) {
      def ~ [A18 <: Adjective[N18], N18] (next: ?^[A18,N18]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)), r: ^[A18,N18]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup18[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18])]) {
      def ~ [A19 <: Adjective[N19], N19] (next: ?^[A19,N19]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)), s: ^[A19,N19]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup19[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19])]) {
      def ~ [A20 <: Adjective[N20], N20] (next: ?^[A20,N20]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19],^[A20,N20])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)), t: ^[A20,N20]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup20[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],A20 <: Adjective[N20],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19,N20] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19],^[A20,N20])]) {
      def ~ [A21 <: Adjective[N21], N21] (next: ?^[A21,N21]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19],^[A20,N20],^[A21,N21])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)), u: ^[A21,N21]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }
    implicit class Tup21[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],A20 <: Adjective[N20],A21 <: Adjective[N21],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19,N20,N21] (v: Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19],^[A20,N20],^[A21,N21])]) {
      def ~ [A22 <: Adjective[N22], N22] (next: ?^[A22,N22]): Either[Set[~^[_,_]], (^[A1,N1],^[A2,N2],^[A3,N3],^[A4,N4],^[A5,N5],^[A6,N6],^[A7,N7],^[A8,N8],^[A9,N9],^[A10,N10],^[A11,N11],^[A12,N12],^[A13,N13],^[A14,N14],^[A15,N15],^[A16,N16],^[A17,N17],^[A18,N18],^[A19,N19],^[A20,N20],^[A21,N21],^[A22,N22])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)), v: ^[A22,N22]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v))
          case (Left(fails), x) => Left(fails ++ ?^.nonMembershipAsSet(x))
          case (Right(_), x)    => Left(?^.nonMembershipAsSet(x))
        }
    }

  }
}

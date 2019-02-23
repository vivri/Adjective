package net.vivri.almostfp

import net.vivri.almostfp.?^.{^, ~^}

import scala.util.Try

/**
  * Represents the "narrowing down" (i.e. refining) of the base type T.
  *
  * Contains precise types for dependent-type polymorphism in the client code,
  * As well as the boolean algebra operations.
  *
  * See [NarrowSpec] `Usage example` for a complete, up-to-date, example on usage.
  */
sealed trait Narrow[T] {
  /**
    * The type of the value
    */
  type V = T

  /**
    * The precise type of this narrowing
    */
  type N <: Narrow[T]

  /**
    * The precise type of success of evaluating this type against a value
    */
  type ^^ = ^[this.N,this.V]

  /**
    * The precise type of failure of evaluating this type against a value
    */
  type ~~ = ~^[this.N,this.V]

  /**
    * Negate/invert the type constraints
    *
    * Can be used without the `.`, e.g.: `~ThisRule`
    */
  def unary_~ : Narrow[T]

  /**
    * Or/Union with another set of type constraints over the same domain
    */
  def | (expr: Narrow[T]): Narrow[T]

  /**
    * And/Intersect with another set of type constraints over the same domain
    */
  def & (expr: Narrow[T]): Narrow[T]

  /**
    * XOR with another set of type constraints over the same domain
    */
  def X (expr: Narrow[T]): Narrow[T]

  /**
    * Validate a value against the constraints.
    *
    * The result should either be `this.^^` if successful, or `this.~~` if failed
    */
  def ?^ (v: T): ?^[N,T]
}

object Narrow {
  type Rule[T] = T => Boolean

  /**
    * Represents a composite expression of type `Narrow`
    */
  trait Expr[T] extends Narrow[T] {
    def unary_~ : Narrow[T] = NOT(this)

    def | (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => OR(this, e)
      case c: ><[T] => OR(this, ID(c))
    }

    def & (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => AND(this, e)
      case c: ><[T] => AND(this, ID(c))
    }

    def X (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => XOR(this, e)
      case c: ><[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[Expr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  /**
    * Represents the atomic building block of narrowing down T.
    *
    * `><[T]` should be read as `Narrowing T` or `Narrowing rule for T`
    */
  class ><[T](val rule: Narrow.Rule[T]) extends Narrow[T] {

    override type N = this.type

    private val id = ID(this)

    override def ?^ (value: T): ?^[N,T] = rule(value) match {
      case true  => ^(this, value)
      case false => ~^(this, value)
    }

    def unary_~ : Narrow[T] = NOT(id)

    def | (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => OR(id, e)
      case c: ><[T] => OR(id, ID(c))
    }

    def & (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => AND(id, e)
      case c: ><[T] => AND(id, ID(c))
    }

    def X (b: Narrow[T]): Narrow[T] = b match {
      case e: Expr[T] => XOR(id, e)
      case c: ><[T] => XOR(id, ID(c))
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

  case class ID[T](constraint: ><[T]) extends Expr[T] {
    override type N = this.type
    override def ?^(v: T) =
      if (constraint rule v) ^(this, v)
      else                   ~^(this, v)

    override val pprint: String = constraint.toString
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
  * Represents the result of an application of a [Narrow] algebraic type expression to a value.
  */
sealed trait ?^[N <: Narrow[T], T] {
  val constraint : N
  val * : T
  val pprint: String

  lazy val lift : Either[~^[N,T], ^[N,T]] = this match {
    case x: ^[N,T] => Right(x)
    case x: ~^[N,T] => Left(x)
  }

  def ~ [N2 <: Narrow[T2], T2] (next: ?^[N2,T2]): Either[Set[~^[_,_]],(^[N,T], ^[N2,T2])] =
    (this, next) match {
      case (a: ^[N, T], b: ^[N2, T2]) => Right((a, b))
      case (a,b) => Left(?^.failAsSet(a) ++ ?^.failAsSet(b))
    }

  override def toString: String = pprint

  override def equals(o: scala.Any): Boolean =
    Try { o.asInstanceOf[?^[N,T]].pprint == pprint } getOrElse false

  override def hashCode(): Int = pprint.hashCode
}

object ?^ {

  def apply[N <: Narrow[T], T](constraint: N, value: T): ?^[constraint.N, T] = constraint ?^ value

  def unapply[N <: Narrow[T], T](arg: ?^[N,T]): Option[(N,T)] = Some((arg.constraint,arg.*))

  case class ^[N <: Narrow[T], T] private[?^] (constraint : N, * : T) extends ?^[N, T] {
    override val pprint: String = s"{ ${*} ∈ ${constraint.toString} }"
  }

  case class ~^[N <: Narrow[T], T] private[?^] (constraint : N, * : T) extends ?^[N, T] {
    override val pprint: String = s"{ ${*} ∉ ${constraint.toString} }"
  }

  private def failAsSet[N <: Narrow[T], T] (x: ?^[N,T]): Set[~^[_,_]] = x match {
    case f: ~^[N,T] => Set(f)
    case _          => Set.empty
  }

  object DSL {
    // Generated from NarrowSpec

    implicit class Tup2[N1 <: Narrow[T1],N2 <: Narrow[T2],T1,T2] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2])]) {
      def ~ [N3 <: Narrow[T3], T3] (next: ?^[N3,T3]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3])] =
        (v, next) match {
          case (Right((a,b)), c: ^[N3,T3]) => Right((a,b,c))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup3[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],T1,T2,T3] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3])]) {
      def ~ [N4 <: Narrow[T4], T4] (next: ?^[N4,T4]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4])] =
        (v, next) match {
          case (Right((a,b,c)), d: ^[N4,T4]) => Right((a,b,c,d))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup4[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],T1,T2,T3,T4] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4])]) {
      def ~ [N5 <: Narrow[T5], T5] (next: ?^[N5,T5]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5])] =
        (v, next) match {
          case (Right((a,b,c,d)), e: ^[N5,T5]) => Right((a,b,c,d,e))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup5[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],T1,T2,T3,T4,T5] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5])]) {
      def ~ [N6 <: Narrow[T6], T6] (next: ?^[N6,T6]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6])] =
        (v, next) match {
          case (Right((a,b,c,d,e)), f: ^[N6,T6]) => Right((a,b,c,d,e,f))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup6[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],T1,T2,T3,T4,T5,T6] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6])]) {
      def ~ [N7 <: Narrow[T7], T7] (next: ?^[N7,T7]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f)), g: ^[N7,T7]) => Right((a,b,c,d,e,f,g))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup7[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],T1,T2,T3,T4,T5,T6,T7] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7])]) {
      def ~ [N8 <: Narrow[T8], T8] (next: ?^[N8,T8]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g)), h: ^[N8,T8]) => Right((a,b,c,d,e,f,g,h))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup8[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],T1,T2,T3,T4,T5,T6,T7,T8] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8])]) {
      def ~ [N9 <: Narrow[T9], T9] (next: ?^[N9,T9]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h)), i: ^[N9,T9]) => Right((a,b,c,d,e,f,g,h,i))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup9[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],T1,T2,T3,T4,T5,T6,T7,T8,T9] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9])]) {
      def ~ [N10 <: Narrow[T10], T10] (next: ?^[N10,T10]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i)), j: ^[N10,T10]) => Right((a,b,c,d,e,f,g,h,i,j))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup10[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10])]) {
      def ~ [N11 <: Narrow[T11], T11] (next: ?^[N11,T11]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j)), k: ^[N11,T11]) => Right((a,b,c,d,e,f,g,h,i,j,k))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup11[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11])]) {
      def ~ [N12 <: Narrow[T12], T12] (next: ?^[N12,T12]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k)), l: ^[N12,T12]) => Right((a,b,c,d,e,f,g,h,i,j,k,l))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup12[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12])]) {
      def ~ [N13 <: Narrow[T13], T13] (next: ?^[N13,T13]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l)), m: ^[N13,T13]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup13[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13])]) {
      def ~ [N14 <: Narrow[T14], T14] (next: ?^[N14,T14]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m)), n: ^[N14,T14]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup14[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14])]) {
      def ~ [N15 <: Narrow[T15], T15] (next: ?^[N15,T15]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n)), o: ^[N15,T15]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup15[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15])]) {
      def ~ [N16 <: Narrow[T16], T16] (next: ?^[N16,T16]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)), p: ^[N16,T16]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup16[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16])]) {
      def ~ [N17 <: Narrow[T17], T17] (next: ?^[N17,T17]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)), q: ^[N17,T17]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup17[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],N17 <: Narrow[T17],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17])]) {
      def ~ [N18 <: Narrow[T18], T18] (next: ?^[N18,T18]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)), r: ^[N18,T18]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup18[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],N17 <: Narrow[T17],N18 <: Narrow[T18],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18])]) {
      def ~ [N19 <: Narrow[T19], T19] (next: ?^[N19,T19]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)), s: ^[N19,T19]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup19[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],N17 <: Narrow[T17],N18 <: Narrow[T18],N19 <: Narrow[T19],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19])]) {
      def ~ [N20 <: Narrow[T20], T20] (next: ?^[N20,T20]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19],^[N20,T20])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)), t: ^[N20,T20]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup20[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],N17 <: Narrow[T17],N18 <: Narrow[T18],N19 <: Narrow[T19],N20 <: Narrow[T20],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19],^[N20,T20])]) {
      def ~ [N21 <: Narrow[T21], T21] (next: ?^[N21,T21]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19],^[N20,T20],^[N21,T21])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)), u: ^[N21,T21]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup21[N1 <: Narrow[T1],N2 <: Narrow[T2],N3 <: Narrow[T3],N4 <: Narrow[T4],N5 <: Narrow[T5],N6 <: Narrow[T6],N7 <: Narrow[T7],N8 <: Narrow[T8],N9 <: Narrow[T9],N10 <: Narrow[T10],N11 <: Narrow[T11],N12 <: Narrow[T12],N13 <: Narrow[T13],N14 <: Narrow[T14],N15 <: Narrow[T15],N16 <: Narrow[T16],N17 <: Narrow[T17],N18 <: Narrow[T18],N19 <: Narrow[T19],N20 <: Narrow[T20],N21 <: Narrow[T21],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] (v: Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19],^[N20,T20],^[N21,T21])]) {
      def ~ [N22 <: Narrow[T22], T22] (next: ?^[N22,T22]): Either[Set[~^[_,_]], (^[N1,T1],^[N2,T2],^[N3,T3],^[N4,T4],^[N5,T5],^[N6,T6],^[N7,T7],^[N8,T8],^[N9,T9],^[N10,T10],^[N11,T11],^[N12,T12],^[N13,T13],^[N14,T14],^[N15,T15],^[N16,T16],^[N17,T17],^[N18,T18],^[N19,T19],^[N20,T20],^[N21,T21],^[N22,T22])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)), v: ^[N22,T22]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
  }

}

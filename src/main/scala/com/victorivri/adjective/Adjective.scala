package com.victorivri.adjective

import com.victorivri.adjective.AdjectiveMembership.{Includes, Excludes}

import scala.util.Try

/**
  * Represents the precise idea of an entity in a particular domain.
  *
  * For example, represents "First Name" instead of "String", or "Database Id" instead of "Int"
  *
  * Enforces membership rules at runtime through predicates and Boolean Algebra.
  *
  * See [AdjectiveSpec] `Usage example` for a complete, up-to-date, example of intended usage.
  */
sealed trait Adjective[T] {
  /**
    * The type of the value
    */
  type Noun = T

  /**
    * The precise type of this instance
    */
  type Type <: Adjective[T]


  /**
    * The precise type of AdjectiveMembership as applies to this Adjective
    */
  type ? = this.Type AdjectiveMembership this.Noun

  /**
    * The precise type of success of evaluating this Adjective against a value
    */
  type ^ = this.Type Includes this.Noun

  /**
    * The precise type of failure of evaluating this Adjective against a value
    */
  type ~ = this.Type Excludes this.Noun

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
    * The result should either be `this.^` if successful, or `this.~~` if failed
    */
  def mightDescribe (v: T): AdjectiveMembership[Type,T]
}

object Adjective {
  type InclusionRule[T] = T => Boolean

  /**
    * Represents a composite expression of type `Adjective`
    */
  sealed trait Expr[T] extends Adjective[T] {
    def unary_~ : Adjective[T] = NOT(this)

    def | (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => OR(this, e)
      case c: Adjective.Nuanced[T] => OR(this, ID(c))
    }

    def & (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => AND(this, e)
      case c: Adjective.Nuanced[T] => AND(this, ID(c))
    }

    def X (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => XOR(this, e)
      case c: Adjective.Nuanced[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[Expr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  /**
    * Represents the atomic building block of [Adjective].
    */
  class Nuanced[T](val rule: InclusionRule[T]) extends Adjective[T] {

    override type Type = this.type

    private val id = ID(this)

    override def mightDescribe (value: T): AdjectiveMembership[Type,T] = rule(value) match {
      case true  => Includes(this, value)
      case false => Excludes(this, value)
    }

    def unary_~ : Adjective[T] = NOT(id)

    def | (b: Adjective[T]): Adjective[T] = b match {
      case e: Adjective.Expr[T] => OR(id, e)
      case c: Adjective.Nuanced[T] => OR(id, ID(c))
    }

    def & (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => AND(id, e)
      case c: Adjective.Nuanced[T] => AND(id, ID(c))
    }

    def X (b: Adjective[T]): Adjective[T] = b match {
      case e: Expr[T] => XOR(id, e)
      case c: Adjective.Nuanced[T] => XOR(id, ID(c))
    }

    lazy val pprint = id.pprint
  }

  case class T[Q]() extends Expr[Q] {
    override type Type = this.type
    override def mightDescribe(v: Q) = Includes(this, v)
    override val pprint: String = "Q"
  }

  case class F[Q]() extends Expr[Q] {
    override type Type = this.type
    override def mightDescribe(v: Q) = Excludes(this, v)
    override val pprint: String = "F"
  }

  case class ID[T](adjective: Adjective.Nuanced[T]) extends Expr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) =
      if (adjective rule v) Includes(this, v)
      else                  Excludes(this, v)

    override val pprint: String = adjective.toString
  }

  case class NOT[T](expr: Expr[T]) extends Expr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) = {
      expr mightDescribe v match {
        case Includes(_,v)  => Excludes(this, v)
        case Excludes(_,v) => Includes(this, v)
      }
    }

    override val pprint: String = s"~${expr.pprint}"
  }

  case class OR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) =
      (exprA mightDescribe v, exprB mightDescribe v) match {
        case (Excludes(_,_), Excludes(_,_)) =>
          Excludes(this, v)
        case _ =>
          Includes(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} | ${exprB.pprint})"
  }

  case class AND[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) =
      (exprA mightDescribe v, exprB mightDescribe v) match {
        case (Includes(_,_), Includes(_,_)) =>
          Includes(this, v)
        case _ =>
          Excludes(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} & ${exprB.pprint})"
  }

  case class XOR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) = {
      (exprA mightDescribe v, exprB mightDescribe v) match {
        case (Includes(_,_), Includes(_,_)) =>
          Excludes(this, v)
        case (Excludes(_,_), Excludes(_,_)) =>
          Excludes(this, v)
        case _ =>
          Includes(this,v)
      }
    }

    override val pprint: String = s"(${exprA.pprint} ⊕ ${exprB.pprint})"
  }
}

/**
  * Represents the idea of value membership in an [Adjective].
  */
sealed trait AdjectiveMembership[N <: Adjective[T], T] {
  val adjective : N
  val base : T
  val pprint: String

  lazy val lift : Either[Excludes[N,T], Includes[N,T]] = this match {
    case x: Includes[N,T] => Right(x)
    case x: Excludes[N,T] => Left(x)
  }

  def ~ [N2 <: Adjective[T2], T2] (next: AdjectiveMembership[N2,T2]): Either[List[Excludes[_,_]],(Includes[N,T], Includes[N2,T2])] =
    (this, next) match {
      case (a: Includes[N, T], b: Includes[N2, T2]) => Right((a, b))
      case (a,b) => Left(AdjectiveMembership.nonMembershipAsList(a) ::: AdjectiveMembership.nonMembershipAsList(b))
    }

  override def toString: String = pprint

  override def equals(o: scala.Any): Boolean =
    Try { o.asInstanceOf[AdjectiveMembership[N,T]].pprint == pprint } getOrElse false

  override def hashCode(): Int = pprint.hashCode
}

object AdjectiveMembership {

  def apply[N <: Adjective[T], T](adjective: N, value: T): adjective.? = adjective mightDescribe value

  def unapply[N <: Adjective[T], T](arg: AdjectiveMembership[N,T]): Option[(N,T)] = Some((arg.adjective,arg.base))

  /**
    * Represents membership of the value `*` in adjective `Type`
    */
  case class Includes[N <: Adjective[T], T] (adjective : N, base : T) extends AdjectiveMembership[N, T] {
    override val pprint: String = s"{ ${base} ∈ ${adjective.toString} }"
  }

  /**
    * Represents non-membership of the value `*` in adjective `Type`
    */
  case class Excludes[N <: Adjective[T], T] (adjective : N, base : T) extends AdjectiveMembership[N, T] {
    override val pprint: String = s"{ ${base} ∉ ${adjective.toString} }"
  }

  protected[adjective] def nonMembershipAsList[N <: Adjective[T], T] (x: AdjectiveMembership[N,T]): List[Excludes[_,_]] = x match {
    case f: Excludes[N,T] => List(f)
    case _                => List.empty
  }
}

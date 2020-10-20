package com.victorivri.adjective

import com.victorivri.adjective.AdjectiveMembership.{Excludes, Includes}

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
sealed trait AdjectiveBase[T] {
  /**
    * The type of the value
    */
  type Noun = T

  /**
    * The precise type of this instance
    */
  type Type <: AdjectiveBase[T]


  /**
    * The precise type of AdjectiveMembership as applies to this AdjectiveBase
    */
  type MembershipCheck = this.Type AdjectiveMembership this.Noun

  /**
    * The precise type of success of evaluating this AdjectiveBase against a value
    */
  type ^ = this.Type Includes this.Noun

  /**
    * The precise type of failure of evaluating this AdjectiveBase against a value
    */
  type ~ = this.Type Excludes this.Noun

  /**
    * Negate/invert the type constraints
    *
    * Can be used without the `.`, e.g.: `~ThisRule`
    */
  def unary_~ : AdjectiveBase[T]

  /**
    * Or/Union with another set of type constraints over the same domain
    */
  def | (expr: AdjectiveBase[T]): AdjectiveBase[T]

  /**
    * And/Intersect with another set of type constraints over the same domain
    */
  def & (expr: AdjectiveBase[T]): AdjectiveBase[T]

  /**
    * XOR with another set of type constraints over the same domain
    */
  def <+> (expr: AdjectiveBase[T]): AdjectiveBase[T]

  /**
    * Validate a value against the constraints.
    *
    * The result should either be `this.^` if successful, or `this.~~` if failed
    */
  def mightDescribe (v: T): MembershipCheck
}

object AdjectiveBase {
  type Nuance[T] = T => Boolean

  /**
    * Represents a composite expression of type `AdjectiveBase`
    */
  sealed trait AdjectiveExpr[T] extends AdjectiveBase[T] {
    override def unary_~ : AdjectiveBase[T] = NOT(this)

    override def | (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveExpr[T] => OR(this, e)
      case c: AdjectiveBase.Adjective[T] => OR(this, ID(c))
    }

    override def & (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveExpr[T] => AND(this, e)
      case c: AdjectiveBase.Adjective[T] => AND(this, ID(c))
    }

    override def <+> (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveExpr[T] => XOR(this, e)
      case c: AdjectiveBase.Adjective[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[AdjectiveExpr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  /**
    * Represents the atomic building block of [AdjectiveBase].
    */
  class Adjective[T](val rule: Nuance[T]) extends AdjectiveBase[T] {

    override type Type = this.type

    private val id = ID(this)

    override def mightDescribe (value: T): MembershipCheck = rule(value) match {
      case true  => Includes(this, value)
      case false => Excludes(this, value)
    }

    override def unary_~ : AdjectiveBase[T] = NOT(id)

    override def | (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveBase.AdjectiveExpr[T] => OR(id, e)
      case c: AdjectiveBase.Adjective[T] => OR(id, ID(c))
    }

    override def & (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveExpr[T] => AND(id, e)
      case c: AdjectiveBase.Adjective[T] => AND(id, ID(c))
    }

    override def <+> (b: AdjectiveBase[T]): AdjectiveBase[T] = b match {
      case e: AdjectiveExpr[T] => XOR(id, e)
      case c: AdjectiveBase.Adjective[T] => XOR(id, ID(c))
    }
  }

  case class T[Q]() extends AdjectiveExpr[Q] {
    override type Type = this.type
    override def mightDescribe(v: Q) = Includes(this, v)
    override val pprint: String = "Q"
  }

  case class F[Q]() extends AdjectiveExpr[Q] {
    override type Type = this.type
    override def mightDescribe(v: Q) = Excludes(this, v)
    override val pprint: String = "F"
  }

  case class ID[T](adjective: AdjectiveBase.Adjective[T]) extends AdjectiveExpr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) =
      if (adjective rule v) Includes(this, v)
      else                  Excludes(this, v)

    override val pprint: String = adjective.toString
  }

  case class NOT[T](expr: AdjectiveExpr[T]) extends AdjectiveExpr[T] {
    override type Type = this.type
    override def mightDescribe(v: T) = {
      expr mightDescribe v match {
        case Includes(_,v)  => Excludes(this, v)
        case Excludes(_,v) => Includes(this, v)
      }
    }

    override val pprint: String = s"~${expr.pprint}"
  }

  case class OR[T](exprA: AdjectiveExpr[T], exprB: AdjectiveExpr[T]) extends AdjectiveExpr[T] {
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

  case class AND[T](exprA: AdjectiveExpr[T], exprB: AdjectiveExpr[T]) extends AdjectiveExpr[T] {
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

  case class XOR[T](exprA: AdjectiveExpr[T], exprB: AdjectiveExpr[T]) extends AdjectiveExpr[T] {
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
  * Represents the idea of value membership in an [AdjectiveBase].
  */
sealed trait AdjectiveMembership[N <: AdjectiveBase[T], T] {
  val adjective : N
  val base : T
  val pprint: String

  lazy val toEither: Either[Excludes[N,T], Includes[N,T]] = this match {
    case x: Includes[N,T] => Right(x)
    case x: Excludes[N,T] => Left(x)
  }

  lazy val toOption: Option[Includes[N,T]] =
    this match {
      case x: Includes[N,T] => Some(x)
      case _                => None
    }

  def ~ [N2 <: AdjectiveBase[T2], T2] (next: AdjectiveMembership[N2,T2]): Either[List[Excludes[_,_]],(Includes[N,T], Includes[N2,T2])] =
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

  def apply[N <: AdjectiveBase[T], T](adjective: N, value: T): adjective.MembershipCheck = adjective mightDescribe value

  def unapply[N <: AdjectiveBase[T], T](arg: AdjectiveMembership[N,T]): Option[(N,T)] = Some((arg.adjective,arg.base))

  /**
    * Represents membership of the value `*` in adjective `Type`
    */
  case class Includes[N <: AdjectiveBase[T], T] (adjective : N, base : T) extends AdjectiveMembership[N, T] {
    override val pprint: String = s"{ ${base} ∈ ${adjective.toString} }"

    /**
      * Applies the same [AdjectiveBase] to the result of the transformation
      */
    def map (fn: T => T) = adjective mightDescribe fn(base)
  }

  /**
    * Represents non-membership of the value `*` in adjective `Type`
    */
  case class Excludes[N <: AdjectiveBase[T], T] (adjective : N, base : T) extends AdjectiveMembership[N, T] {
    override val pprint: String = s"{ ${base} ∉ ${adjective.toString} }"
  }

  protected[adjective] def nonMembershipAsList[N <: AdjectiveBase[T], T] (x: AdjectiveMembership[N,T]): List[Excludes[_,_]] = x match {
    case f: Excludes[N,T] => List(f)
    case _                => List.empty
  }
}

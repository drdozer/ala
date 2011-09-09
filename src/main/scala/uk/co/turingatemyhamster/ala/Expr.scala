package uk.co.turingatemyhamster.ala

import annotation.implicitNotFound

/** A terminal atom representing a position. */
sealed trait Term {
  def asString: String
  def equivalent(other: Term): Boolean
}

/** A terminal atom representing a known integer constant. */
case class Const(i: Int) extends Term {
  def asString = i.toString
  def equivalent(other: Term) = other match {
    case Const(j) => i == j
    case _ => false
  }
}

/** A variable. */
sealed trait Var extends Term

/** A named variable. */
case class NamedVar(id: String) extends Var {
  def asString = "?" + id
  def equivalent(other: Term) = other match {
    case NamedVar(td) => id == td
    case _ => false
  }
}

/** An anonymous variable. */
case class AnonVar() extends Var {
  def asString = "?"
  def equivalent(other: Term) = this equals other
}

/** An expression. */
sealed trait Expr[OP <: BinOp]

/** Negation. */
case class Negate[OP <: BinOp](x: Expr[OP]) extends Expr[NegationOf[OP]]

/** Conjunction. */
case class And[X <: BinOp, Y <: BinOp](x: Expr[X], y: Expr[Y]) extends Expr[ConjunctionOf[X, Y]]

/** Binary expression over 2 terms. */
case class BinExpr[OP <: BinOp](a: Term, b: Term, op: OP) extends Expr[OP]

/** Logical negation */
case class NegationOf[OP <: BinOp]() extends BinOp

/** Logical conjunction */
case class ConjunctionOf[X <: BinOp, Y <: BinOp]() extends BinOp

/** Operation between two terms. */
sealed trait BinOp

/** Less than. */
sealed trait LessThan extends BinOp
/** Less than. */
case object LessThan extends LessThan

/** Less than or equal to. */
sealed trait LessThanOrEqualTo extends BinOp
/** Less than or equal to. */
case object LessThanOrEqualTo extends LessThanOrEqualTo

/** Equal to. */
sealed trait EqualTo extends BinOp
/** Equal to. */
case object EqualTo extends EqualTo

/** Equal to or greater than. */
sealed trait EqualToOrGreaterThan extends BinOp
/** Equal to or greater than. */
case object EqualToOrGreaterThan extends EqualToOrGreaterThan

/** Greater than. */
sealed trait GreaterThan extends BinOp
/** Greater than. */
case object GreaterThan extends GreaterThan

/** Next. a+1 = b. Next |- LessThan */
sealed trait Next extends BinOp
/** Next. a+1 = b. Next |- LessThan */
case object Next extends Next


object Expr {
  
  // logicals
  def not[OP <: BinOp](x: Expr[OP]) = Negate(x)
  def and[X <: BinOp, Y <: BinOp](x: Expr[X], y: Expr[Y]) = And(x, y)

  // relations
  def lessThan(a: Term, b: Term): BinExpr[LessThan] = BinExpr(a, b, LessThan)
  def lessThanOrEqualTo(a: Term, b: Term): BinExpr[LessThanOrEqualTo] = BinExpr(a, b, LessThanOrEqualTo)
  def equalTo(a: Term, b: Term): BinExpr[EqualTo] = BinExpr(a, b, EqualTo)
  def equalToOrGreaterThan(a: Term, b: Term): BinExpr[EqualToOrGreaterThan] = BinExpr(a, b, EqualToOrGreaterThan)
  def greaterThan(a: Term, b: Term): BinExpr[GreaterThan] = BinExpr(a, b, GreaterThan)
  def next(a: Term, b: Term): BinExpr[Next] = BinExpr(a, b, Next)

}

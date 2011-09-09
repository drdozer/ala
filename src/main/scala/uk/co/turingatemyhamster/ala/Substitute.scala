package uk.co.turingatemyhamster.ala

import annotation.implicitNotFound


@implicitNotFound(msg = "Can not substitute terms in expressions of type: ${OP}.")
trait Substitute[OP <: BinOp] {
  def apply(t: Term, u: Term, expr: Expr[OP]): Expr[OP]
}

object Substitute {

  /** Substitute one term for another given a target term. */
  def substitute(t: Term, u: Term, v: Term): Term = if(v equivalent t) u else v

  /** Substitute one term for another in an expression. */
  def substitute[OP <: BinOp](t: Term, u: Term, expr: Expr[OP])(implicit s: Substitute[OP]) = s(t, u, expr)


  import Expr._

  /** Substitute terms in a negative expression. */
  implicit def substituteInNegate[OP <: BinOp](implicit s: Substitute[OP]): Substitute[NegationOf[OP]] = new Substitute[NegationOf[OP]] {
    def apply(t: Term, u: Term, expr: Expr[NegationOf[OP]]): Expr[NegationOf[OP]] = expr match {
      case Negate(x) => not(s(t, u, x))
    }
  }

  /** Substitute terms in a conjunctive expression. */
  implicit def substituteInAnd[X <: BinOp, Y <: BinOp](implicit xs: Substitute[X], ys: Substitute[Y]): Substitute[ConjunctionOf[X, Y]] =
    new Substitute[ConjunctionOf[X, Y]] {
    def apply(t: Term, u: Term, expr: Expr[ConjunctionOf[X, Y]]): Expr[ConjunctionOf[X, Y]] = expr match {
      case And(x, y) => and(xs(t, u, x), ys(t, u, y))
    }
  }

  /** Substitute terms in a binary operator expression. */
  implicit def substituteInBinOp[OP <: BinOp]: Substitute[OP] = new Substitute[OP] {
    def apply(t: Term, u: Term, expr: Expr[OP]): Expr[OP] = expr match {
      case BinExpr(a, b, op) => BinExpr(substitute(t, u, a), substitute(t, u, b), op)
      // no Negate case as this should be handled by substituteInNegate
      // no ConjunctionOf case as this should be handled by substituteInAnd
    }
  }

}

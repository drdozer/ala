package uk.co.turingatemyhamster.ala

import annotation.implicitNotFound


@implicitNotFound(msg = "Could not minimze ${X} to ${Y}")
trait Minimalizer[X <: BinOp, Y <: BinOp] {
  def apply(x: Expr[X])(implicit yIsMinimal: MinimalForm[Y]): Expr[Y]
}

object Minimalizer {

  import Expr._

  /** If x is already minimal, return x. */
  implicit def alreadyMinimal[X <: BinOp]
  (implicit xIsMinimal: MinimalForm[X]): Minimalizer[X, X] = new Minimalizer[X, X] {
    def apply(x: Expr[X])(implicit xIsMinimal: MinimalForm[X]) = x
  }

  /** a LessThanOrEqualTo b ~> E x . a Next x ^ x LessThan b. */
  implicit def minimiseLessThanOrEqualTo: Minimalizer[LessThanOrEqualTo, ConjunctionOf[Next, LessThan]] =
    new Minimalizer[LessThanOrEqualTo, ConjunctionOf[Next, LessThan]]
    {
      def apply(x: Expr[LessThanOrEqualTo])
               (implicit yIsMinimal: MinimalForm[ConjunctionOf[Next, LessThan]]): Expr[ConjunctionOf[Next, LessThan]] = x match {
        case BinExpr(a, b, LessThanOrEqualTo) =>
          val v = AnonVar()
          and(next(a, v), lessThan(v, b))
      }
    }

  /** a EqualToOrGreaterThan b ~> E x . b LessThan x ^ x Next a */
  implicit def minimizeEqualToOrGreaterThan: Minimalizer[EqualToOrGreaterThan, ConjunctionOf[LessThan, Next]] =
    new Minimalizer[EqualToOrGreaterThan, ConjunctionOf[LessThan, Next]]
    {
      def apply(x: Expr[EqualToOrGreaterThan])
               (implicit yIsMinimal: MinimalForm[ConjunctionOf[LessThan, Next]]): Expr[ConjunctionOf[LessThan, Next]] = x  match {
        case BinExpr(a, b, EqualToOrGreaterThan) =>
          val v = AnonVar()
          and(lessThan(a, v), next(v, b))
      }
    }

  /** a GreaterThan b ~> b LessThan a. */
  implicit def minimizeGreaterThan: Minimalizer[GreaterThan, LessThan] =
    new Minimalizer[GreaterThan, LessThan]
    {
      def apply(x: Expr[GreaterThan])
               (implicit yIsMinimal: MinimalForm[LessThan]): Expr[LessThan] = x match {
        case BinExpr(a, b, GreaterThan) => lessThan(b, a)
      }
    }
}
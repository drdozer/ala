package uk.co.turingatemyhamster.ala

import annotation.implicitNotFound

/** Evidence of well-formedness of an expression. */
@implicitNotFound(msg = "Unambe to find a way to represent predicate ${P} as an expression of type ${OP}.")
trait AsExpr[P <: AlaPredicate, OP <: BinOp] {
  def apply(a: (Term, Term), b: (Term, Term)): Expr[OP]
}


object AsExpr {
  import Expr._

  /** If (a, b) are a first/last pair, then a <= b. */
  def asExpr(a: Term, b: Term): Expr[LessThanOrEqualTo] = lessThanOrEqualTo(a, b)

  /** Represent an ALA triple as an equivalent expression. */
  def asExpr[P <: AlaPredicate, OP <: BinOp]
  (a: (Term, Term), p: P, b: (Term, Term))
  (implicit ae: AsExpr[P, OP]) = ae.apply(a, b)

  import AlaPredicate._

  implicit def beforeAsExpr: AsExpr[Before.type, LessThan] = new AsExpr[Before.type, LessThan] {
    def apply(a: (Term, Term), b: (Term, Term)): BinExpr[LessThan] = lessThan(a._2, b._1)
  }

  implicit def afterAsExpr: AsExpr[After.type, LessThan] = new AsExpr[After.type, LessThan] {
    def apply(a: (Term, Term), b: (Term, Term)): BinExpr[LessThan] = lessThan(b._2, a._1)
  }

  implicit def meetsAsExpr: AsExpr[Meets.type, ConjunctionOf[Next, EqualTo]] =
    new AsExpr[Meets.type, ConjunctionOf[Next, EqualTo]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[Next, EqualTo]] =
      {
        val x = AnonVar()
        and(next(x, a._2), equalTo(x, b._1))
      }
    }

  implicit def followsAsExpr: AsExpr[Follows.type, ConjunctionOf[Next, EqualTo]] =
    new AsExpr[Follows.type, ConjunctionOf[Next, EqualTo]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[Next, EqualTo]] =
      {
        val x = AnonVar()
        and(next(x, b._2), equalTo(x, a._1))
      }
    }

  implicit def overlapsAsExpr: AsExpr[Overlaps.type, ConjunctionOf[NegationOf[LessThan], NegationOf[LessThan]]] =
    new AsExpr[Overlaps.type, ConjunctionOf[NegationOf[LessThan], NegationOf[LessThan]]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[NegationOf[LessThan], NegationOf[LessThan]]] =
        and(not(lessThan(a._2, b._1)), not(lessThan(b._2, a._1)))
    }

  implicit def sameFirstAsExpr: AsExpr[SameFirst.type, EqualTo] = new AsExpr[SameFirst.type, EqualTo] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[EqualTo] = equalTo(a._1, b._1)
  }

  implicit def sameLastAsExpr: AsExpr[SameLast.type, EqualTo] = new AsExpr[SameLast.type, EqualTo] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[EqualTo] = equalTo(a._2, b._2)
  }

  implicit def withinAsExpr: AsExpr[Within.type, ConjunctionOf[EqualToOrGreaterThan, LessThanOrEqualTo]] =
    new AsExpr[Within.type, ConjunctionOf[EqualToOrGreaterThan, LessThanOrEqualTo]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[EqualToOrGreaterThan, LessThanOrEqualTo]] =
        and(equalToOrGreaterThan(a._1, b._1), lessThanOrEqualTo(a._2, b._2))
    }

  implicit def containsAsExpr: AsExpr[Contains.type, ConjunctionOf[LessThanOrEqualTo, EqualToOrGreaterThan]] =
    new AsExpr[Contains.type, ConjunctionOf[LessThanOrEqualTo, EqualToOrGreaterThan]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[LessThanOrEqualTo, EqualToOrGreaterThan]] =
        and(lessThanOrEqualTo(a._1, b._1), equalToOrGreaterThan(a._2, b._2))
    }

  implicit def equalAsExpr: AsExpr[Equal.type, ConjunctionOf[EqualTo, EqualTo]] =
    new AsExpr[Equal.type, ConjunctionOf[EqualTo, EqualTo]]
    {
      def apply(a: (Term, Term), b: (Term, Term)): Expr[ConjunctionOf[EqualTo, EqualTo]] =
        and(equalTo(a._1, b._1), equalTo(a._2, b._2))
    }
}

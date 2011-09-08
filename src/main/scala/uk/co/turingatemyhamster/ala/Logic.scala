package uk.co.turingatemyhamster.ala

/** Evidence of well-formedness of an expression. */
trait Evidence[A, B]

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

sealed trait Var extends Term

/** A terminal atom representing an unknown, together with a unique ID. */
case class NamedVar(id: String) extends Var {
  def asString = "?" + id
  def equivalent(other: Term) = other match {
    case NamedVar(td) => id == td
    case _ => false
  }
}

case class AnonVar() extends Var {
  def asString = "?"
  def equivalent(other: Term) = this equals other
}

/** An expression. */
sealed trait Expr[OP]

/** Negation. */
case class Negate[OP <: BinOp](x: Expr[OP]) extends Expr[NegationOf[OP]]

/** Conjunction. */
case class And[X <: BinOp, Y <: BinOp](x: Expr[X], y: Expr[Y]) extends Expr[(X, Y)]

/** Binary expression over 2 terms. */
case class BinExpr[OP <: BinOp](a: Term, b: Term, op: OP) extends Expr[OP]

/** Logical negation */
case class NegationOf[OP <: BinOp]() extends BinOp

/** Operation between two terms. */
sealed trait BinOp

/** Less than. */
case object LessThan extends BinOp

/** Less than or equal to. */
case object LessThanOrEqualTo extends BinOp

/** Equal to. */
case object EqualTo extends BinOp

/** Equal to or greater than. */
case object EqualToOrGreaterThan extends BinOp

/** Greater than. */
case object GreaterThan extends BinOp

/** Next. a+1 = b. */
case object Next extends BinOp


object Expr {
  def not[OP <: BinOp](x: Expr[OP])= Negate(x)
  def and[X <: BinOp, Y <: BinOp](x: Expr[X], y: Expr[Y]) = And(x, y)

  def lessThan(a: Term, b: Term): BinExpr[LessThan.type] = BinExpr(a, b, LessThan)
  def lessThanOrEqualTo(a: Term, b: Term) = BinExpr(a, b, LessThanOrEqualTo)
  def equalTo(a: Term, b: Term) = BinExpr(a, b, EqualTo)
  def equalToOrGreaterThan(a: Term, b: Term) = BinExpr(a, b, EqualToOrGreaterThan)
  def greaterThan(a: Term, b: Term) = BinExpr(a, b, GreaterThan)
  def next(a: Term, b: Term) = BinExpr(a, b, Next)


  trait AsExpr[P <: AlaPredicate, OP] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[OP]
  }

  /** If (a, b) are a first/last pair, then a <= b. */
  def asExpr(a: Term, b: Term): Expr[LessThanOrEqualTo.type] = lessThanOrEqualTo(a, b)

  /** Represent an ALA tripple as an equivalent expression. */
  def asExpr[P <: AlaPredicate, E <: Expr[OP], OP](a: (Term, Term), p: P, b: (Term, Term))
                                                  (implicit ae: AsExpr[P, E]) = ae.apply(a, b)

  import AlaPredicate._

  implicit def beforeAsExpr: AsExpr[Before.type, LessThan.type] = new AsExpr[Before.type, LessThan.type] {
    def apply(a: (Term, Term), b: (Term, Term)): BinExpr[LessThan.type] = lessThan(a._2, b._1)
  }

  implicit def afterAsExpr: AsExpr[After.type, LessThan.type] = new AsExpr[After.type, LessThan.type] {
    def apply(a: (Term, Term), b: (Term, Term)): BinExpr[LessThan.type] = lessThan(b._2, a._1)
  }

  implicit def meetsAsExpr: AsExpr[Meets.type, (Next.type , EqualTo.type)] = new AsExpr[Meets.type, (Next.type, EqualTo.type)] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(Next.type, EqualTo.type)] = {
      val x = AnonVar()
      and(next(x, a._2), equalTo(x, b._1))
    }
  }

  implicit def followsAsExpr: AsExpr[Follows.type, (Next.type, EqualTo.type)] = new AsExpr[Follows.type, (Next.type, EqualTo.type)] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(Next.type, EqualTo.type)] = {
      val x = AnonVar()
      and(next(x, b._2), equalTo(x, a._1))
    }
  }

  implicit def overlapsAsExpr: AsExpr[Overlaps.type, (NegationOf[LessThan.type], NegationOf[LessThan.type])] = new AsExpr[Overlaps.type, (NegationOf[LessThan.type], NegationOf[LessThan.type])] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(NegationOf[LessThan.type], NegationOf[LessThan.type])] =
      and(not(lessThan(a._2, b._1)), not(lessThan(b._2, a._1)))
  }

  implicit def sameFirstAsExpr: AsExpr[SameFirst.type, EqualTo.type] = new AsExpr[SameFirst.type, EqualTo.type] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[EqualTo.type] = equalTo(a._1, b._1)
  }

  implicit def sameLastAsExpr: AsExpr[SameLast.type, EqualTo.type] = new AsExpr[SameLast.type, EqualTo.type] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[EqualTo.type] = equalTo(a._2, b._2)
  }

  implicit def withinAsExpr: AsExpr[Within.type, (EqualToOrGreaterThan.type, LessThanOrEqualTo.type)] = new AsExpr[Within.type, (EqualToOrGreaterThan.type, LessThanOrEqualTo.type)] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(EqualToOrGreaterThan.type, LessThanOrEqualTo.type)] =
      and(equalToOrGreaterThan(a._1, b._1), lessThanOrEqualTo(a._2, b._2))
  }

  implicit def containsAsExpr: AsExpr[Contains.type, (LessThanOrEqualTo.type, EqualToOrGreaterThan.type)] = new AsExpr[Contains.type, (LessThanOrEqualTo.type, EqualToOrGreaterThan.type)] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(LessThanOrEqualTo.type, EqualToOrGreaterThan.type)] =
      and(lessThanOrEqualTo(a._1, b._1), equalToOrGreaterThan(a._2, b._2))
  }

  implicit def equalAsExpr: AsExpr[Equal.type, (EqualTo.type, EqualTo.type)] = new AsExpr[Equal.type, (EqualTo.type, EqualTo.type)] {
    def apply(a: (Term, Term), b: (Term, Term)): Expr[(EqualTo.type, EqualTo.type)] =
      and(equalTo(a._1, b._1), equalTo(a._2, b._2))
  }

  trait Substitute[OP] {
    def apply(t: Term, u: Term, expr: Expr[OP]): Expr[OP]
  }

  def substitute[OP](t: Term, u: Term, expr: Expr[OP])(implicit s: Substitute[OP]) = s(t, u, expr)

  /** Substitute terms in a negative expression. */
  implicit def substituteInNegate[OP <: BinOp](implicit s: Substitute[OP]): Substitute[NegationOf[OP]] = new Substitute[NegationOf[OP]] {
    def apply(t: Term, u: Term, expr: Expr[NegationOf[OP]]): Expr[NegationOf[OP]] = expr match {
      case Negate(x) => not(s(t, u, x))
    }
  }

  /** Substitute terms in a conjunctive expression. */
  implicit def substituteInAnd[X <: BinOp, Y <: BinOp](implicit xs: Substitute[X], ys: Substitute[Y]): Substitute[(X, Y)] = new Substitute[(X, Y)] {
    def apply(t: Term, u: Term, expr: Expr[(X, Y)]): Expr[(X, Y)] = expr match {
      case And(x, y) => and(xs(t, u, x), ys(t, u, y))
    }
  }

  /** Substitute terms in a binary operator expression. */
  implicit def substituteInBinOp[OP <: BinOp]: Substitute[OP] = new Substitute[OP] {
    def apply(t: Term, u: Term, expr: Expr[OP]): Expr[OP] = expr match {
      case BinExpr(a, b, op) => BinExpr(substitute(t, u, a), substitute(t, u, b), op)
      // no Negate case as this should be handled by substituteInNegate
    }
  }

  /** Substitute one term for another given a target term. */
  def substitute(t: Term, u: Term, v: Term): Term = if(v equivalent t) u else v
}
package uk.co.turingatemyhamster.ala

import annotation.implicitNotFound


/** Evidence of minimal-form expressions. */
@implicitNotFound(msg = "${OP} is not in minimal form.")
trait MinimalForm[OP <: BinOp]

object MinimalForm {

  /** LessThan is minimal. */
  implicit def lessThanIsMinimal: MinimalForm[LessThan] = new MinimalForm[LessThan] {}

  /** EqualTo is minimal. */
  implicit def equalToIsMinimal: MinimalForm[EqualTo] = new MinimalForm[EqualTo] {}

  /** Next is minimal. */
  implicit def nextIsMinimal: MinimalForm[Next] = new MinimalForm[Next] {}

  /** And is minimal if both args are minimal. */
  implicit def andIsMinimal[X <: BinOp, Y <: BinOp]
  (implicit minX: MinimalForm[X], minY: MinimalForm[Y]): MinimalForm[ConjunctionOf[X, Y]] =
    new AndMinimalForm(minX, minY)

  class AndMinimalForm[X <: BinOp, Y <: BinOp](minX: MinimalForm[X], minY: MinimalForm[Y])
    extends MinimalForm[ConjunctionOf[X, Y]]
}

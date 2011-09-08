package uk.co.turingatemyhamster.ala

/** An Allen's Interval Algebra predicate.
 *
 * This is adapted from http://en.wikipedia.org/wiki/Allen's_Interval_Algebra.
 *
 * Some of the predicate names have been adapted to be more natural for biological sequence locations
 * rather than for temporal intervals.
 */
sealed trait AlaPredicate {
  /** Test two ranges to see if this preicate holds for them.
   *
   * @param a  the first range
   * @param b  the second range
   * @return   true if the preidcate holds, false if it does not
   */
  def holdsFor(a: BioRange, b: BioRange): Boolean

  /** Returns a predicate that represents the arguments being reversed.
   *  For any predicate `p` and `q=p.reverse`, `p(a, b) == p(b,a)` for all `a`, `b`.
   */
  def reverse: AlaPredicate
}

/**
 * The common Allen's Interval Algebra predicates.
 */
object AlaPredicate {

  /** The whole of range a is before range b. */
  object Before extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.last < b.first
    def reverse = After
  }

  /** The whole of range a is after range b. */
  object After extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = Before.holdsFor(b, a)
    def reverse = Before
  }

  /** The start of range b is one after the end of range a. */
  object Meets extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = (a.last + 1) == b.first
    def reverse = Follows
  }

  /** The end of range a is one before the end of range a */
  object Follows extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = Meets.holdsFor(b, a)
    def reverse = Meets
  }

  /** There are some positions shared by both a and b. */
  object Overlaps extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = !Before.holdsFor(a, b) && !After.holdsFor(a, b)
    def reverse = Overlaps
  }

  /** The two ranges start at the same location. */
  object SameFirst extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.first == b.first
    def reverse = SameFirst
  }

  /** The two ranges end at the same location. */
  object SameLast extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.last == b.last
    def reverse = SameLast
  }

  /** The whole of range a is within range b. */
  object Within extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.first >= b.first && a.last <= b.last
    def reverse = Contains
  }

  /** The whole of range b is within range a. */
  object Contains extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = Within.holdsFor(b, a)
    def reverse = Within
  }

  /** The two ranges are identical in extent. */
  object Equal extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.first == b.first && a.last == b.last
    def reverse = Equal
  }
}

/**
 * A biological coordinate range, inclusive of first and last.
 */
trait BioRange {
  /**
   * The first index in this range, inclusive.
   */
  def first: Int

  /**
   * The last index in this range, inclusive.
   */
  def last: Int
}

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
  object before extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.last < b.first
    def reverse = after
  }

  /** The whole of range a is after range b. */
  object after extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = before.holdsFor(b, a)
    def reverse = before
  }

  /** The start of range b is one after the end of range a. */
  object meets extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = (a.last + 1) == b.first
    def reverse = follows
  }

  /** The end of range a is one before the end of range a */
  object follows extends AlaPredicate {
    def holdsFor(a: BioRange, B: BioRange) = meets(b, a)
    def reverse = meets
  }

  /** There are some positions shared by both a and b. */
  object overlaps extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = !before(a, b) && !after(a, b)
    def reverse = overlaps
  }

  /** The two ranges start at the same location. */
  object sameFirst extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.first == b.first
    def reverse = sameFirst
  }

  /** The two ranges end at the same location. */
  object sameLast extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.last == b.last
    def reverse = sameLast
  }

  /** The whole of range a is within range b. */
  object within extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = a.first >= b.first && a.last <= b.last
    def reverse = contains
  }

  /** The whole of range b is within range a. */
  object contains extends AlaPredicate {
    def holdsFor(a: BioRange, b: BioRange) = within(b, a)
    def reverse = within
  }

  /** The two ranges are identical in extent. */
  object equal extends AlaPredicte {
    def holdsFor(a: BioRange, b: BioRange) = a.first == b.first && a.last == b.last
    def reverse = equal
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

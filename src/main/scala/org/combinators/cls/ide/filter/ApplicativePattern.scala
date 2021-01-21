package org.combinators.cls.ide.filter

sealed trait ApplicativePattern{
  def isApplyPattern: Boolean = false

}

/** Denotes any application of combinator or term*/
final case class StarPattern() extends ApplicativePattern {
  override def toString: String = "*"
}
/** Represents combinator */
final case class CombinatorPattern(combinator: String) extends ApplicativePattern{
  override def toString: String = combinator
}

/** Represents the application of a pattern1 to pattern2. */
final case class ApplyPattern(pattern1: ApplicativePattern, pattern2:ApplicativePattern)
  extends ApplicativePattern{
  override def isApplyPattern: Boolean = true

  override def toString: String = s"@(${pattern1}, ${pattern2})"
}

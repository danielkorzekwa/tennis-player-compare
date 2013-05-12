package dk.tennis.compare.rating.trueskill.util

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

import org.junit._
import Assert._

object TrueSkillUtil {

  def assertRating(expected: TrueSkillRating, actual: TrueSkillRating, delta: Double) = {
    assertEquals(expected.mean, actual.mean, delta)
    assertEquals(expected.variance, actual.variance, delta)

  }
}
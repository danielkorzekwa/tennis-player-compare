package dk.tennis.compare.rating.trueskill.testutil

import org.junit._
import Assert._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

object TestUtil {

  def assertRating(expected: TrueSkillRating, actual: TrueSkillRating, delta: Double) = {
    assertEquals(expected.mean, actual.mean, delta)
    assertEquals(expected.variance, actual.variance, delta)
  }

}
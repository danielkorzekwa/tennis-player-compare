package dk.tennis.compare.rating.testutil

import org.junit._
import Assert._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams

object TestUtil {

  def assertRating(expected: TrueSkillRating, actual: TrueSkillRating, delta: Double) = {
    assertEquals(expected.mean, actual.mean, delta)
    assertEquals(expected.variance, actual.variance, delta)
  }

  def assertPlayerSkill(expected: PlayerSkill, actual: PlayerSkill, delta: Double) = {
    assertEquals(expected.mean, actual.mean, delta)
    assertEquals(expected.variance, actual.variance, delta)
  }

  def assertPlayerSkills(expected: PlayerSkills, actual: PlayerSkills, delta: Double) = {
    assertEquals(expected.player, actual.player)
    assertEquals(expected.skillOnServe.mean, actual.skillOnServe.mean, delta)
    assertEquals(expected.skillOnServe.variance, actual.skillOnServe.variance, delta)

    assertEquals(expected.skillOnReturn.mean, actual.skillOnReturn.mean, delta)
    assertEquals(expected.skillOnReturn.variance, actual.skillOnReturn.variance, delta)
  }

  def assertMultiSkillParams(expected: MultiSkillParams, actual: MultiSkillParams, delta: Double) = {
    assertEquals(expected.skillOnServeTransVariance, actual.skillOnServeTransVariance, delta)
    assertEquals(expected.skillOnReturnTransVariance, actual.skillOnReturnTransVariance, delta)
    assertEquals(expected.perfVariance, actual.perfVariance, delta)
  }
}
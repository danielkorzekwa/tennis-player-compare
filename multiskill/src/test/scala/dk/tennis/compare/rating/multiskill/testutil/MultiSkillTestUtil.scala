package dk.tennis.compare.rating.multiskill.testutil

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import org.junit._
import Assert._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats

object MultiSkillTestUtil {

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
    assertEquals(expected.perfVarianceOnServe, actual.perfVarianceOnServe, delta)
    assertEquals(expected.perfVarianceOnReturn, actual.perfVarianceOnReturn, delta)
  }
}
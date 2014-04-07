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
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import dk.bayes.math.gaussian.Gaussian

object MultiSkillTestUtil {

   def assertGaussian(expected: Gaussian, actual: Gaussian, delta: Double) = {
    assertEquals(expected.m, actual.m, delta)
    assertEquals(expected.v, actual.v, delta)
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

}
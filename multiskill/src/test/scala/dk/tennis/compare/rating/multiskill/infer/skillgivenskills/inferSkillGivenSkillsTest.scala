package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import java.util.Date
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc

class inferSkillGivenSkillsTest {

  val day = 1000L * 3600 * 24
  val skillCovFunc = SkillOverTimeCovFunc(List(log(0.3), log(30), log(1), log(365)))

  val skillMeanFunc = (player: Player) => 0.6

  val players = Array(
    Player("p1", "p2", onServe = true, timestamp = new Date(day)),
    Player("p1", "p2", onServe = true, timestamp = new Date(30 * day)),
    Player("p1", "p3", onServe = true, timestamp = new Date(5 * day)))

  val skillsMean = Matrix(0.2, 0.8, -0.1)
  val skillsVar = skillCovFunc.covarianceMatrix(players)

  val skillsGaussian = MultivariateGaussian(skillsMean, skillsVar)

  @Test def test {

    val player = Player("p1", "p5", onServe = true, timestamp = new Date(1 * day))

    val playerSkill = inferSkillGivenSkills(players, skillsGaussian, player, skillCovFunc, skillMeanFunc)
    assertEquals(0.1950, playerSkill.m, 0.0001)
    assertEquals(1.0900, playerSkill.v, 0.0001)
  }

  @Test def test_converge_to_mean_value {

    val player = Player("p1", "p5", onServe = true, timestamp = new Date(1200 * day))

    val playerSkill = inferSkillGivenSkills(players, skillsGaussian, player, skillCovFunc, skillMeanFunc)

    assertEquals(0.6174, playerSkill.m, 0.0001)
    assertEquals(1.0900, playerSkill.v, 0.0001)
  }
}
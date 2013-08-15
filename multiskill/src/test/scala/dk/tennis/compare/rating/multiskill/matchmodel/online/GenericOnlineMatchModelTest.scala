package dk.tennis.compare.rating.multiskill.matchmodel.online

import org.junit._
import org.junit.Assert._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PointResult
import scala.math._
import dk.bayes.learn.lds.TransitionStat
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.GenericLDSLearn
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._

class GenericOnlineMatchModelTest {

  private val p1Skills = PlayerSkills("player1", PlayerSkill(0, 1), PlayerSkill(0, 1))
  private val p2Skills = PlayerSkills("player2", PlayerSkill(0, 1), PlayerSkill(0, 1))

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15

  private val inPlayMultiSkill = GenericOnlineMatchModel(p1Skills, p2Skills, perfVarianceOnServe, perfVarianceOnReturn)

  @Test def test {

    val pointResults = List(PointResult("player1", true), PointResult("player1", true), PointResult("player1", false),
      PointResult("player2", false), PointResult("player2", true), PointResult("player2", true), PointResult("player1", true))

    pointResults.foreach(p => inPlayMultiSkill.onPoint(p))

    assertPlayerSkills(PlayerSkills("player1", PlayerSkill(0.2496, 0.9290), PlayerSkill(-0.1344, 0.9455)), inPlayMultiSkill.getP1Skills(), 0.0001)
    assertPlayerSkills(PlayerSkills("player2", PlayerSkill(0.1344, 0.9455), PlayerSkill(-0.2496, 0.9290)), inPlayMultiSkill.getP2Skills(), 0.0001)

  }

}
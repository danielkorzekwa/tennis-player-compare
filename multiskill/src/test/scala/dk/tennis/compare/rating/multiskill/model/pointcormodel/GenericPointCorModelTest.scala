package dk.tennis.compare.rating.multiskill.model.pointcormodel

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._

class GenericPointCorModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals_zero_skill_covariance {

    val (p1Marginal, p2Marginal, skillCov) = pointModel.skillMarginals(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), true, skillCovariance = 0)

    assertPlayerSkill(PlayerSkill(0.2914, 0.6908), p1Marginal, 0.0001)
    assertPlayerSkill(PlayerSkill(-0.2654, 0.4953), p2Marginal, 0.0001)
    assertEquals(0.0065, skillCov, 0.0001)
  }

  @Test def skillMarginals_positive_skill_covariance {

    val (p1Marginal, p2Marginal, skillCov) = pointModel.skillMarginals(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), true, skillCovariance = 0.3)

    assertPlayerSkill(PlayerSkill(0.2527, 0.6969), p1Marginal, 0.0001)
    assertPlayerSkill(PlayerSkill(-0.2263, 0.4992), p2Marginal, 0.0001)
    assertEquals(0.3015, skillCov, 0.0001)
  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), skillCovariance = 0.0), 0.0001)
    assertEquals(0.528, pointModel.pointProb(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), skillCovariance = 0.45), 0.0001)

    assertEquals(0.7268, pointModel.pointProb(PlayerSkill(1.7, 0.9), PlayerSkill(-1.8, 0.8), skillCovariance = 0.0), 0.0001)
    assertEquals(0.7310, pointModel.pointProb(PlayerSkill(1.7, 0.9), PlayerSkill(-1.8, 0.8), skillCovariance = 0.7), 0.0001)
  }
}
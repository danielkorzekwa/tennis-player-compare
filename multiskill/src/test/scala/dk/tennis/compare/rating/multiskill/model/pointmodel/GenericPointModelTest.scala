package dk.tennis.compare.rating.multiskill.model.pointmodel

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._

class GenericPointModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals {

    val (p1Marginal, p2Marginal) = pointModel.skillMarginals(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), true)
    assertPlayerSkill(PlayerSkill(0.2916, 0.6908), p1Marginal, 0.0001)
    assertPlayerSkill(PlayerSkill(-0.2654, 0.4953), p2Marginal, 0.0001)
  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5)), 0.0001)
  }
}
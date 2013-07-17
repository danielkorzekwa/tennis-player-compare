package dk.tennis.compare.rating.multiskill.pointmodel

import dk.tennis.compare.rating.testutil.TestUtil._
import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

class GenericPointModelTest {

  private val perfVariance = pow(25d / 6, 2)
  private val pointModel = GenericPointModel(perfVariance)

  @Test def skillMarginals {

    assertPlayerSkill(PlayerSkill(0.2882, 0.6915), pointModel.skillMarginals(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), true)._1, 0.0001)
    assertPlayerSkill(PlayerSkill(-0.263, 0.4956), pointModel.skillMarginals(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5), true)._2, 0.0001)
  }

  @Test def pointProb {
    assertEquals(0.5266, pointModel.pointProb(PlayerSkill(0.2, 0.7), PlayerSkill(-0.2, 0.5)), 0.0001)
  }
}
package dk.tennis.compare.rating.multiskill.matchmodel

import org.junit._
import org.junit.Assert._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PointResult
import scala.math._
import dk.tennis.compare.rating.testutil.TestUtil._

class GenericMatchModelTest {

  private val p1Skills = PlayerSkills("player1", PlayerSkill(0, 1), PlayerSkill(0, 1))
  private val p2Skills = PlayerSkills("player2", PlayerSkill(0, 1), PlayerSkill(0, 1))
  private val perfVariance = pow(25d / 6, 2)
  private val inPlayMultiSkill = GenericMatchModel(p1Skills, p2Skills, perfVariance)

  @Test def test {

    val pointResults = List(PointResult("player1", true), PointResult("player1", true), PointResult("player1", false),
      PointResult("player2", false), PointResult("player2", true), PointResult("player2", true), PointResult("player1", true))

    pointResults.foreach(p => inPlayMultiSkill.onPoint(p))

    assertPlayerSkills(PlayerSkills("player1", PlayerSkill(0.2418, 0.9340), PlayerSkill(-0.1294, 0.9494)), inPlayMultiSkill.getP1Skills(), 0.0001)
    assertPlayerSkills(PlayerSkills("player2", PlayerSkill(0.1294, 0.9494), PlayerSkill(-0.2418, 0.9340)), inPlayMultiSkill.getP2Skills(), 0.0001)

  }
}
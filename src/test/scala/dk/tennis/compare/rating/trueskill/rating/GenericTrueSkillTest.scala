package dk.tennis.compare.rating.trueskill.rating

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.util.TrueSkillUtil._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

class GenericTrueSkillTest {

  private val skillTransVariance = pow(25d / 300, 2)
  private val performanceVariance = (pow(25d / 16, 2), pow(25d / 16, 2))
  private val trueSkillModel = GenericTrueSkill(skillTransVariance)

  @Test def test_player1_cares_less_about_point_2 {

    val result1 = Result("player1", "player2", true)
    trueSkillModel.addResult(result1, performanceVariance)

    assertRating(TrueSkillRating(0.305, 0.9133), trueSkillModel.getRatings()("player1"), 0.001)
    assertRating(TrueSkillRating(-0.305, 0.9133), trueSkillModel.getRatings()("player2"), 0.001)

    val result2 = Result("player1", "player2", true, Some(pow(250d / 16, 2)), Some(pow(25d / 16, 2)))
    trueSkillModel.addResult(result2, performanceVariance)

    assertRating(TrueSkillRating(0.351, 0.918), trueSkillModel.getRatings()("player1"), 0.001)
    assertRating(TrueSkillRating(-0.351, 0.918), trueSkillModel.getRatings()("player2"), 0.001)
  }
}
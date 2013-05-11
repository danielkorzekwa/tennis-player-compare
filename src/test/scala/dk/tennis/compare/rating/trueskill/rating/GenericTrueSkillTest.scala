package dk.tennis.compare.rating.trueskill.rating

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result

class GenericTrueSkillTest {

  private val skillTransVariance = pow(25d / 300, 2)
  private val performanceVariance = pow(25d / 16, 2)
  private val trueSkillModel = GenericTrueSkill(skillTransVariance, performanceVariance)

  @Test def test {

    val result1 = Result("player1", "player2", true)
    trueSkillModel.addResult(result1)

    println(trueSkillModel.getRatings()("player1"))
    println(trueSkillModel.getRatings()("player2"))

    val result2 = Result("player1", "player2", true, Some(pow(25d / 160, 2)), Some(pow(25d / 16, 2)))
    trueSkillModel.addResult(result2)

    println(trueSkillModel.getRatings()("player1"))
    println(trueSkillModel.getRatings()("player2"))
  }
}
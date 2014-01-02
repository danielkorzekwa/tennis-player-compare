package dk.tennis.compare

import org.junit._
import org.junit.Assert._
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

class PointAndMatchProbTest {

  //10.397463467905618,0.04835926487252291
  
  val p1OnServe = PlayerSkill(10.48, 0.40)
  val p1OnReturn = PlayerSkill(0, 1)

  val p2OnServe = PlayerSkill(10.397463467905618,0.04835926487252291)
  val p2OnReturn = PlayerSkill(0, 1)

  @Test def test {
    val pointModel = GenericPointModel(200, 200)
    val p1PointProb = pointModel.pointProb(p1OnServe, p2OnReturn)
    val p2PointProb = pointModel.pointProb(p2OnServe, p1OnReturn)

    println("p1_point_prob:" + p1PointProb)
    println("p2_point_prob:" + p2PointProb)
    println("p1_match_prob: " + TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH))
  }
}
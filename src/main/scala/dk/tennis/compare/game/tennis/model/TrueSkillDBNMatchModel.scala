package dk.tennis.compare.game.tennis.model

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import scala.math._
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.trueskill.ratingdbn.GenericTrueSkillDbn
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.pointprob.GenericPointProbCalc
import dk.tennis.compare.game.tennis.domain.TennisResult

case class TrueSkillDBNMatchModel extends GameModel {

  private val skillTransVariance = 0.034233
  private val performanceVariance = (pow(25d / 16, 2), pow(25d / 16, 2))
  private val trueSkillModel = GenericTrueSkillDbn(skillTransVariance, performanceVariance._1) //only a single performance is supported for this model

  def gameProb(r: GameResult): Option[Double] = {

    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.calcRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val winProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get, playerBSkill.get, performanceVariance)

      /**Enhance prob for 5 sets match.*/
      if (r.asInstanceOf[TennisResult].numOfSets == 3) {
        val pointProb = GenericPointProbCalc.calcPointProb(winProb, THREE_SET_MATCH)
        val matchProb = TennisProbFormulaCalc.matchProb(pointProb, pointProb, FIVE_SET_MATCH)
        Some(matchProb)
      } else Some(winProb)
    } else None

    prob
  }

  def addGameResult(r: GameResult) {
    val result = Result(r.player1, r.player2, r.player1Win.get)
    trueSkillModel.addResult(result)
  }
}
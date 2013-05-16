package dk.tennis.compare.game.tennis.model

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.rating.TrueSkill
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.game.tennis.pointprob.GenericPointProbCalc
import dk.tennis.compare.game.tennis.domain.TennisResult

case class TrueSkillGameModel extends GameModel {

  private val skillTransVariance = pow(25d / 30000000, 2)
  /**Tuple2[player1,player2]*/
  private val performanceVariance = (pow(25d / 16, 2), pow(25d / 16, 2))
   val trueSkillModel = GenericTrueSkill(skillTransVariance)

  def gameProb(r: GameResult): Option[Double] = {

    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val winProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get, playerBSkill.get, performanceVariance)

      Some(winProb)

    } else None

    prob
  }

  def addGameResult(r: GameResult) {
    trueSkillModel.addResult(Result(r.player1, r.player2, r.player1Win.get), performanceVariance)
  }

  def getTrueSkillModel(): TrueSkill = trueSkillModel
}
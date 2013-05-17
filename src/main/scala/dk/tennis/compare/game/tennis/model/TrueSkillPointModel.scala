package dk.tennis.compare.game.tennis.model

import dk.tennis.compare.tester.GameModel
import scala.math._
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.trueskill.model.Result

case class TrueSkillPointModel extends GameModel {

  private val skillTransVariance = pow(25d / 3000, 2)
  private val performanceVariance = (pow(250d / 16, 2), pow(250d / 16, 2))
  val trueSkillModel = GenericTrueSkill(skillTransVariance)

  def gameProb(r: GameResult): Option[Double] = {
    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val playerPointProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get, playerBSkill.get, performanceVariance)

      val matchProb = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(playerPointProb, playerPointProb, THREE_SET_MATCH)
      else TennisProbFormulaCalc.matchProb(playerPointProb, playerPointProb, FIVE_SET_MATCH)
 
      Some(matchProb)
    } else None

    prob
  }

  def addGameResult(r: GameResult) = {

    r match {
      case r: TennisResult => {

        r.points.get.foreach { pointWon =>

          trueSkillModel.addResult(Result(r.player1, r.player2, pointWon), performanceVariance)
        }

      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }
}
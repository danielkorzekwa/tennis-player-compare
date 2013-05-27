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
import dk.tennis.compare.rating.trueskill.rating.servereturn.GenericTrueSkillServeReturn

case class TrueSkillPointServeReturnModel extends GameModel {

  private val skillTransVariance = pow(25d / 3000, 2)
  private val performanceVariance = (pow(250d / 16, 2), pow(250d / 16, 2))
  val trueSkillModel = GenericTrueSkillServeReturn(skillTransVariance)

  def gameProb(r: GameResult): Option[Double] = {
    /**key - playerName, value - Tuple2[playerSkillServe,playerSkillReturn]*/
    val ratingsMap: Map[String, Tuple2[TrueSkillRating, TrueSkillRating]] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val p1PointProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get._1, playerBSkill.get._2, performanceVariance)
      val p2PointProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerBSkill.get._1, playerASkill.get._2, performanceVariance)

      val matchProb = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
      else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

      Some(matchProb)
    } else None

    prob
  }

  def addGameResult(r: GameResult) = {

    r match {
      case r: TennisResult => {

        r.points.get.foreach { point =>

          if (point.playerOnServe.equals(r.player1))
            trueSkillModel.addResult(Result(r.player1, r.player2, point.won), performanceVariance)
          else if (point.playerOnServe.equals(r.player2))
            trueSkillModel.addResult(Result(r.player2, r.player1, point.won), performanceVariance)
          else throw new IllegalArgumentException("Player on serve not found")
        }

      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }
}
package dk.tennis.compare.game.tennis.model

import dk.tennis.compare.tester.GameModel
import scala.math._
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.GenericMultiSkill
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel

case class MultiSkillModel extends GameModel {

  private val skillTransVariance = pow(25d / 150, 2)
  private val performanceVariance = (pow(250d / 16, 2))

  val multiSkillModel = GenericMultiSkill(skillTransVariance, performanceVariance)

  def gameProb(r: GameResult): Option[Double] = {
    /**key - playerName, value - player skills*/
    val playerSkills: Map[String, PlayerSkills] = multiSkillModel.getSkills()

    val playerASkill = playerSkills.get(r.player1)
    val playerBSkill = playerSkills.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val p1PointProb = GenericPointModel(performanceVariance).pointProb(playerASkill.get.skillOnServe, playerBSkill.get.skillOnReturn)
      val p2PointProb = GenericPointModel(performanceVariance).pointProb(playerBSkill.get.skillOnServe, playerASkill.get.skillOnReturn)

      val matchProb = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
      else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

      Some(matchProb)
    } else None

    prob

  }

  def addGameResult(r: GameResult) = {

    r match {
      case r: TennisResult => {

        val pointResults = r.points.get.map { point =>
          if (point.playerOnServe.equals(r.player1))
            PointResult(r.player1, point.won)
          else if (point.playerOnServe.equals(r.player2))
            PointResult(r.player2, point.won)
          else throw new IllegalArgumentException("Player on serve not found")
        }

        multiSkillModel.processTennisMatch(r.player1, r.player2, pointResults)

      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }
}
package dk.tennis.compare.game.twopointsgame.model

import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.game.twopointsgame.TwoPointsGameResult
import dk.tennis.compare.game.twopointsgame.TwoPointsGameResult
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.game.twopointsgame.TwoPointsGame
import dk.tennis.compare.game.twopointsgame.TwoPointsGame

case class TrueSkillPointModel extends GameModel {

  private val skillTransVariance = pow(25d / 300, 2)
  private val performanceVariance = (pow(25d / 16, 2), pow(25d / 16, 2))
  private val trueSkillModel = GenericTrueSkill(skillTransVariance)

  def gameProb(r: GameResult): Option[Double] = {
    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val pointProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get, playerBSkill.get, performanceVariance)

      val matchProb = TwoPointsGame(0, 0).gameProb(gameState => pointProb)

      Some(matchProb)

    } else None

    prob
  }

  def addGameResult(r: GameResult) = {
    r match {
      case r: TwoPointsGameResult => {

        r.points.foreach(pointWon => trueSkillModel.addResult(Result(r.player1, r.player2, pointWon), performanceVariance))

      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }
}
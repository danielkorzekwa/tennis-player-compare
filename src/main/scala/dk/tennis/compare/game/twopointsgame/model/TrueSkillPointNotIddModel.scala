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
import dk.tennis.compare.game.twopointsgame.TwoPointsGame

case class TrueSkillPointNotIddModel extends GameModel {

  private val skillTransVariance = pow(25d / 30000000, 2)
  val trueSkillModel = GenericTrueSkill(skillTransVariance)

  private val perfVariance = (game: TwoPointsGame) => game match {
    case TwoPointsGame(0, 0) => (5d, 5d)
    case TwoPointsGame(1, 0) => (3d, 0.5d)
    case TwoPointsGame(0, 1) => (0.5d, 3d)
    case TwoPointsGame(1, 1) => (0.1d, 0.1d)
  }

  def gameProb(r: GameResult): Option[Double] = {
    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(r.player1)
    val playerBSkill = ratingsMap.get(r.player2)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val matchProb = TwoPointsGame(0, 0).gameProb { gameState =>
        val (p1PerfVar, p2PerfVar) = perfVariance(gameState)
        val pointProb = GenericTrueSkillMatchProb(skillTransVariance).matchProb(playerASkill.get, playerBSkill.get, (p1PerfVar, p2PerfVar))
        pointProb
      }
      Some(matchProb)

    } else None

    prob
  }

  def addGameResult(r: GameResult) = {

    var game = TwoPointsGame(0, 0)
    r match {
      case r: TwoPointsGameResult => {

        r.points.foreach { pointWon =>

          trueSkillModel.addResult(Result(r.player1, r.player2, pointWon), perfVariance(game))
          game = game.score(pointWon)
        }
      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }
}
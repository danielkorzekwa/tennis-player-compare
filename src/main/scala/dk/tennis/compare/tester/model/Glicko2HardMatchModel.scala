package dk.tennis.compare.tester.model

import dk.tennis.compare.rating.glicko2.GenericGlicko2Rating
import dk.tennis.compare.rating.glicko2.Glicko2Rating._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._
import java.util._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.simulation.game.GameResult

case class Glicko2HardMatchModel extends GameModel {

  val glicko2 = new GenericGlicko2Rating(0, 100d / 173.7178, 0.03, 0.2, 3)

  def gameProb(r: GameResult): Option[Double] = {

    val ratings = glicko2.getRatings()
    val defaultRating = PlayerRating(Rating(0.5, Double.MaxValue, Double.MaxValue, new Date(r.timestamp.get)), Rating(0.5, Double.MaxValue, Double.MaxValue, new Date(r.timestamp.get)))
    val ratingA = ratings.getOrElse(r.player1, defaultRating)
    val ratingB = ratings.getOrElse(r.player2, defaultRating)

    val playerAOnServeProb = GenericGlicko2Rating.E(ratingA.ratingOnServe.rating, ratingB.ratingOnReturn.rating, ratingB.ratingOnReturn.deviation)
    val playerBOnServeProb = GenericGlicko2Rating.E(ratingB.ratingOnServe.rating, ratingA.ratingOnReturn.rating, ratingA.ratingOnReturn.deviation)

    if (!ratingA.ratingOnServe.rating.isNaN() &&
      !ratingB.ratingOnServe.rating.isNaN() &&
      !ratingA.ratingOnReturn.rating.isNaN() &&
      !ratingB.ratingOnReturn.rating.isNaN() &&
      ratingA.ratingOnServe.deviation < 0.6 &&
      ratingB.ratingOnServe.deviation < 0.6 &&
      ratingA.ratingOnReturn.deviation < 0.6 &&
      ratingB.ratingOnReturn.deviation < 0.6)
      Some(playerAOnServeProb) else None
  }

  def addGameResult(r: GameResult) {

    val playerAWinner: Int = if (r.player1Win.get) 1 else 0

    val result =
      Result(r.player1, r.player2, playerAWinner, new Date(r.timestamp.get))

    glicko2.sendResult(result)
  }
}
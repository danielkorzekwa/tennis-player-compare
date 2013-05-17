package dk.tennis.compare.game.tennis.model

import dk.tennis.compare.rating.glicko2.GenericGlicko2Rating
import dk.tennis.compare.rating.glicko2.Glicko2Rating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._
import java.util._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.domain.TennisResult

case class Glicko2MatchModel extends GameModel {

  val glicko2 = new GenericGlicko2Rating(0, 100d / 173.7178, 0.06, 0.05, 3)

  def gameProb(r: GameResult): Option[Double] = {

    val ratings = glicko2.getRatings()
    val defaultRating = PlayerRating(Rating(0.5, Double.MaxValue, Double.MaxValue, r.timestamp.get), Rating(0.5, Double.MaxValue, Double.MaxValue, r.timestamp.get))
    val ratingA = ratings.getOrElse(r.player1, defaultRating)
    val ratingB = ratings.getOrElse(r.player2, defaultRating)

    val playerAOnServeProb = GenericGlicko2Rating.E(ratingA.ratingOnServe.rating, ratingB.ratingOnReturn.rating, ratingB.ratingOnReturn.deviation)
    val playerBOnServeProb = GenericGlicko2Rating.E(ratingB.ratingOnServe.rating, ratingA.ratingOnReturn.rating, ratingA.ratingOnReturn.deviation)

    val matchProbAGivenB = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, FIVE_SET_MATCH)
    /**Theta and beta are learned with logistic regression. Prediction variable = matchProbAGivenB, predicted variable = match outcome.*/
    val matchProbAGivenBTuned = 1 / (1 + exp(-4 * (matchProbAGivenB - 0.5)))

    if (!ratingA.ratingOnServe.rating.isNaN() &&
      !ratingB.ratingOnServe.rating.isNaN() &&
      !ratingA.ratingOnReturn.rating.isNaN() &&
      !ratingB.ratingOnReturn.rating.isNaN())
      Some(matchProbAGivenBTuned) else None
  }

  def addGameResult(r: GameResult) {

    val tennisResult = r.asInstanceOf[TennisResult]

    val results =
      Result(tennisResult.player1, tennisResult.player2,
        tennisResult.player1ServicePointsWonPct.get, tennisResult.timestamp.get) ::
        Result(tennisResult.player2, tennisResult.player1,
          tennisResult.player2ServicePointsWonPct.get, tennisResult.timestamp.get) :: Nil

    results.foreach(r => glicko2.sendResult(r))
  }
}
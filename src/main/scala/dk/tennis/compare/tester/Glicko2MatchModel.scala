package dk.tennis.compare.tester

import dk.tennis.compare.rating.glicko2.GenericGlicko2Rating
import dk.tennis.compare.rating.glicko2.Glicko2Rating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._
import java.util._
import dk.atp.api.domain.MatchComposite

case class Glicko2MatchModel extends MatchModel {

  val glicko2 = new GenericGlicko2Rating(0, 100d / 173.7178, 0.06, 0.5, 14)

  def matchProb(playerA: String, playerB: String, timestamp: Long): Option[Double] = {

    val ratings = glicko2.getRatings()
    val defaultRating = PlayerRating(Rating(0.5, Double.MaxValue, Double.MaxValue, new Date(timestamp)), Rating(0.5, Double.MaxValue, Double.MaxValue, new Date(timestamp)))
    val ratingA = ratings.getOrElse(playerA, defaultRating)
    val ratingB = ratings.getOrElse(playerB, defaultRating)

    val playerAOnServeProb = GenericGlicko2Rating.E(ratingA.ratingOnServe.rating, ratingB.ratingOnReturn.rating, ratingB.ratingOnReturn.deviation)
    val playerBOnServeProb = GenericGlicko2Rating.E(ratingB.ratingOnServe.rating, ratingA.ratingOnReturn.rating, ratingA.ratingOnReturn.deviation)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)
    /**Theta and beta are learned with logistic regression. Prediction variable = matchProbAGivenB, predicted variable = match outcome.*/
    val matchProbAGivenBTuned = 1 / (1 + exp(-(-1.5502 + 3.1003 * matchProbAGivenB)))

    if (!ratingA.ratingOnServe.rating.isNaN() &&
      !ratingB.ratingOnServe.rating.isNaN() &&
      !ratingA.ratingOnReturn.rating.isNaN() &&
      !ratingB.ratingOnReturn.rating.isNaN() &&
      ratingA.ratingOnServe.deviation < 0.5 &&
      ratingB.ratingOnServe.deviation < 0.5 &&
      ratingA.ratingOnReturn.deviation < 0.5 &&
      ratingB.ratingOnReturn.deviation < 0.5)
      Some(matchProbAGivenBTuned) else None
  }

  def addMatchResult(m: MatchComposite) {
    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    val results =
      Result(playerAFacts.playerName, playerBFacts.playerName,
        playerAFacts.totalServicePointsWonPct, m.tournament.tournamentTime) ::
        Result(playerBFacts.playerName, playerAFacts.playerName,
          playerBFacts.totalServicePointsWonPct, m.tournament.tournamentTime) :: Nil

    results.foreach(r => glicko2.sendResult(r))
  }
}
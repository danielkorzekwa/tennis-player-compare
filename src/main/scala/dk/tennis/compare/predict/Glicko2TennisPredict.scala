package dk.tennis.compare.predict

import dk.atp.api.domain.MatchComposite
import TennisPredict._
import dk.tennis.compare.glicko2.GenericGlicko2Rating
import dk.tennis.compare.glicko2._
import Glicko2Rating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.Math._
import dk.tennis.compare.predict._
/**
 * Predicts tennis match winner for a set of matches.
 */
object Glicko2TennisPredict extends TennisPredict {

  def predict(matches: Seq[MatchComposite], matchFilter: (MatchComposite) => Boolean): Seq[PredictionRecord] = {

    implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
    val glicko2 = new GenericGlicko2Rating(0, 350d / 173.7178, 0.06, 0.5, 7)

    //@return rating before processing new tennis result Tuple2[ratingPlayerA, ratingPlayerB]
    def updateRatings(m: MatchComposite): Tuple2[PlayerRating, PlayerRating] = {
      val playerAFacts = m.matchFacts.playerAFacts
      val playerBFacts = m.matchFacts.playerBFacts

      val ratings = glicko2.getRatings()
      val defaultRating = PlayerRating(Rating(0.5, Double.MaxValue, Double.MaxValue, m.tournament.tournamentTime), Rating(0.5, Double.MaxValue, Double.MaxValue, m.tournament.tournamentTime))
      val ratingA = ratings.getOrElse(playerAFacts.playerName, defaultRating)
      val ratingB = ratings.getOrElse(playerBFacts.playerName, defaultRating)

      val results =
        Result(playerAFacts.playerName, playerBFacts.playerName,
          playerAFacts.totalServicePointsWonPct, m.tournament.tournamentTime) ::
          Result(playerBFacts.playerName, playerAFacts.playerName,
            playerBFacts.totalServicePointsWonPct, m.tournament.tournamentTime) :: Nil

      results.foreach(r => glicko2.sendResult(r))

      (ratingA, ratingB)
    }

    val predictionRecords = for {
      m <- matches

      val (ratingA, ratingB) = updateRatings(m)

      val playerAOnServeProb = GenericGlicko2Rating.E(ratingA.ratingOnServe.rating, ratingB.ratingOnReturn.rating, ratingB.ratingOnReturn.deviation)
      val playerBOnServeProb = GenericGlicko2Rating.E(ratingB.ratingOnServe.rating, ratingA.ratingOnReturn.rating, ratingA.ratingOnReturn.deviation)

      val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)
      /**Theta and beta are learned with logisitc regression. Prediction variable = matchProbAGivenB, predicted variable = match outcome.*/
      val matchProbAGivenBTuned = 1 / (1 + exp(-(-1.5502 + 3.1003 * matchProbAGivenB)))

      if (!ratingA.ratingOnServe.rating.isNaN() &&
        !ratingB.ratingOnServe.rating.isNaN() &&
        !ratingA.ratingOnReturn.rating.isNaN() &&
        !ratingB.ratingOnReturn.rating.isNaN() &&
        ratingA.ratingOnServe.deviation < 0.5 &&
        ratingB.ratingOnServe.deviation < 0.5 &&
        ratingA.ratingOnReturn.deviation < 0.5 &&
        ratingB.ratingOnReturn.deviation < 0.5)
       
    } yield PredictionRecord(
      m.tournament.tournamentTime, m.matchFacts.playerAFacts.playerName,
      m.matchFacts.playerBFacts.playerName,
      matchProbAGivenBTuned,
      m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName))

    predictionRecords

  }
}
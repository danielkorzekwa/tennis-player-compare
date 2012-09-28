package dk.tennis.compare.predict

import TennisPredict._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.markov._
import MarkovRating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.Math._
/**
 * Predicts tennis match winner for a set of matches.
 */
object MarkovTennisPredict extends TennisPredict {

  implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0

  def predict(matches: Seq[MatchComposite], matchFilter: (MatchComposite) => Boolean, progress: (PredictionRecord) => Unit): Seq[PredictionRecord] = {

    // def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double =
    //   playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.02 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))

    val startRating = 1
    val endRating = 50
    val markovRating = new GenericMarkovRating(startRating, endRating, calculateWinProbOnServe)

    //@return rating before processing new tennis result Tuple2[ratingPlayerA, ratingPlayerB]
    def updateRatings(m: MatchComposite): Tuple2[PlayerRating, PlayerRating] = {
      val playerAFacts = m.matchFacts.playerAFacts
      val playerBFacts = m.matchFacts.playerBFacts

      val ratings = markovRating.getRatings()
      val defaultRating = PlayerRating(Rating.generateRatings(startRating, endRating), Rating.generateRatings(startRating, endRating))
      val ratingA = ratings.getOrElse(playerAFacts.playerName, defaultRating)
      val ratingB = ratings.getOrElse(playerBFacts.playerName, defaultRating)

      val results =
        Result(playerAFacts.playerName, playerBFacts.playerName,
          playerAFacts.totalServicePointsWon, playerAFacts.totalServicePoints - playerAFacts.totalServicePointsWon, m.tournament.tournamentTime) ::
          Result(playerBFacts.playerName, playerAFacts.playerName,
            playerBFacts.totalServicePointsWon, playerBFacts.totalServicePoints - playerBFacts.totalServicePointsWon, m.tournament.tournamentTime) :: Nil

      results.foreach(r => markovRating.sendResult(r))

      (ratingA, ratingB)
    }

    val matchesSize = matches.size
    val predictionRecords: Seq[PredictionRecord] = for ((m, index) <- matches.zipWithIndex; if (matchFilter(m))) yield {

      println("Predicting winner of tennis match  (Markov model) - %d / %d".format(index, matchesSize))

      val (ratingA, ratingB) = updateRatings(m)

      val playerAOnServeProb = markovRating.calcWinProb(ratingA.ratingOnServe, ratingB.ratingOnReturn)
      val playerBOnServeProb = markovRating.calcWinProb(ratingB.ratingOnServe, ratingA.ratingOnReturn)

      val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)

      val predictionRecord = PredictionRecord(
        m.tournament.tournamentTime, m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        matchProbAGivenB,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName))

      progress(predictionRecord)
      predictionRecord
    }

    predictionRecords
  }
}
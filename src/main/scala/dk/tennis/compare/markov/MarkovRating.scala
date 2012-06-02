package dk.tennis.compare.markov

import MarkovRating._
import java.util.Date

/**
 * Calculates tennis player rating based on Markov localisation.
 *
 */
object MarkovRating {

  object Rating {
    def generateRatings(startRatingValue: Int, endRatingValue: Int): List[Rating] = {
      val numOfRatings = endRatingValue - startRatingValue + 1
      val ratings = (startRatingValue to numOfRatings).map(i => Rating(i, 1d / numOfRatings))
      ratings.toList
    }
  }
  case class Rating(val ratingValue: Int, val ratingProb: Double) {
    override def toString = "Rating [ratingValue=%s,ratingProb=%s]".format(ratingValue, ratingProb)
  }

  case class PlayerRating(ratingOnServe: Rating, ratingOnReturn: Rating)
  
  /**
   * @param playerA
   * @param playerB
   * @param playerAPointsWon/playerAPointsLost Number of points won/lost by player A against player B.
   * @param timestamp
   */
  case class Result(playerA: String, playerB: String,  playerAPointsWon: Int, playerAPointsLost: Int, timestamp: Date)
}
trait MarkovRating {

  /**
   * Calculates new ratings for players A and B given the evidence (number of points won/lost on serve by player A against player B in a tennis match).
   *
   * @param playerARatingOnServe List of rating values and probabilities representing player A rating on serve.
   *
   * @param playerBRatingOnServe List of rating values and probabilities representing player B rating on return.
   *
   * @param playerAPointsWon/playerAPointsLost Number of points won/lost by player A against player B.
   *
   * @param calculateWinProbability Function, that calculates win probability on serve by player A against player B. (playerARatingOnServe, playerBRatingOReturn) => winProbability.
   *
   * @return Tuple2[New rating on serve for player A, New rating on return for player B]
   */
  def calcRatings(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating],
    playerAPointsWon: Int, playerAPointsLost: Int, calculateWinProbOnServe: (Int, Int) => Double): Tuple2[List[Rating], List[Rating]]

  /**Calculates point on serve winning probability by player A against player B.*/
  def calcWinProb(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating], calculateWinProbOnServe: (Int, Int) => Double): Double

  /**
   * Calculates tennis player ratings based on a history of tennis results
   * @return Map[playerName, playerRating]
   */
  def calcPlayerRatings(results: List[Result]): Map[String, PlayerRating]
}
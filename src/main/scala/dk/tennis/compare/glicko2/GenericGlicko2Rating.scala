package dk.tennis.compare.glicko2

import Glicko2Rating._
import java.util.Date
import GenericGlicko2Rating._
import Math._

object GenericGlicko2Rating {

  /**Player A plays against player B.*/

  def g(deviationB: Double): Double = 1 / sqrt(1 + 3 * pow(deviationB, 2) / pow(Pi, 2))

  def E(ratingA: Double, ratingB: Double, deviationB: Double) = 1d / (1 + exp(-g(deviationB) * (ratingA - ratingB)))

  def variance(g: Double, E: Double): Double = pow(pow(g, 2) * E * (1 - E), -1)

  def delta(variance: Double, g: Double, score: Double, E: Double): Double = variance * g * (score - E)

  def deviationGivenVolatility(deviationA: Double, volatility: Double): Double = sqrt(pow(deviationA, 2) + pow(volatility, 2))

  def newDeviation(deviationA: Double, variance: Double) = {
    1 / sqrt(1 / pow(deviationA, 2) + 1 / variance)
  }

  def newRatingValue(ratingA: Double, deviationA: Double, g: Double, score: Double, E: Double) = ratingA + pow(deviationA, 2) * g * (score - E)

  def newRating(ratingA: Rating, ratingB: Rating, score: Double, timestamp: Date): Rating = {
    val g = GenericGlicko2Rating.g(ratingB.deviation)
    val E = GenericGlicko2Rating.E(ratingA.rating, ratingB.rating, ratingB.deviation)
    val variance = GenericGlicko2Rating.variance(g, E)
    val delta = GenericGlicko2Rating.delta(variance, g, score, E)
    val newVolatility = ratingA.volatility
    val preRatingPeriodDeviation = GenericGlicko2Rating.deviationGivenVolatility(ratingA.deviation, newVolatility)

    val newDeviation = GenericGlicko2Rating.newDeviation(preRatingPeriodDeviation, variance)
    val newRatingValue = GenericGlicko2Rating.newRatingValue(ratingA.rating, newDeviation, g, score, E)
    Rating(newRatingValue, newDeviation, newVolatility, timestamp)
  }

}
class GenericGlicko2Rating(initialRating: Double = 0, initialDeviation: Double = 350d / 173.7178, initialVolatility: Double = 0.06) extends Glicko2Rating {

  /**
   * @return Map[playerName, playerRating]
   */
  def calcServeReturnRatings(results: List[Result]): Map[String, PlayerRating] = {
    /**Map[player,Rating*/
    val ratings = results.foldLeft(Map[String, PlayerRating]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  private def updateRatings(ratings: Map[String, PlayerRating], result: Result): Map[String, PlayerRating] = {
    val currRatingA = ratings.getOrElse(result.playerA, initRating(result.timestamp))
    val currRatingB = ratings.getOrElse(result.playerB, initRating(result.timestamp))

    val newRatingAOnServe = newRating(currRatingA.ratingOnServe, currRatingB.ratingOnReturn, result.score, result.timestamp)
    val newRatingBOnReturn = newRating(currRatingB.ratingOnReturn, currRatingA.ratingOnServe, 1 - result.score, result.timestamp)

    val newRatingA = currRatingA.copy(ratingOnServe = newRatingAOnServe)
    val newRatingB = currRatingB.copy(ratingOnReturn = newRatingBOnReturn)

    val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)
    newRatings
  }

  /**Tuple2[rating on serve, rating on return]*/
  private def initRating(timestamp: Date) = PlayerRating(
    Rating(initialRating, initialDeviation, initialVolatility, timestamp),
    Rating(initialRating, initialDeviation, initialVolatility, timestamp))
}
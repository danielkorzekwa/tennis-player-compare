package dk.tennis.compare.glicko2

import Glicko2Rating._
import java.util.Date
import GenericGlicko2Rating._
import Math._
import scala.annotation.tailrec
object GenericGlicko2Rating {

  /**Player A plays against player B.*/

  def g(deviationB: Double): Double = 1 / sqrt(1 + 3 * pow(deviationB, 2) / pow(Pi, 2))

  def E(ratingA: Double, ratingB: Double, deviationB: Double) = 1d / (1 + exp(-g(deviationB) * (ratingA - ratingB)))

  def variance(g: Double, E: Double): Double = pow(pow(g, 2) * E * (1 - E), -1)

  def delta(variance: Double, g: Double, score: Double, E: Double): Double = variance * g * (score - E)

  def newVolatility(deviationA: Double, volatilityA: Double, variance: Double, delta: Double, tau: Double): Double = {
    val a = log(pow(volatilityA, 2))

    val A = a

    def f(x: Double) =
      exp(x) * (pow(delta, 2) - pow(deviationA, 2) - variance - exp(x)) /
        2 * pow(pow(deviationA, 2) + variance + exp(x), 2) -
        (x - a) / pow(tau, 2)

    @tailrec
    def calcB(k: Int): Double = {
      val factor = a - k * sqrt(pow(tau, 2))
      if (f(factor) < 0) calcB(k + 1) else factor
    }

    val B = if (pow(delta, 2) > (pow(deviationA, 2) + variance)) log(pow(delta, 2) - pow(deviationA, 2) - variance) else calcB(1)

    val epsilon = 0.000001

    @tailrec
    def convergeA(A: Double, B: Double, fA: Double, fB: Double): Double = {

      if (abs((B - A)) > epsilon) {
        val C = A + (A - B) * fA / (fB - fA)
        val (newA, newFA) = if (f(C) * fB < 0) (B, fB) else (A, fA / 2)
        val (newB, newFB) = (C, f(C))
        convergeA(newA, newB, newFA, newFB)
      } else A
    }

    val convergedA = convergeA(A, B, f(A), f(B))

    val newVolatility = exp(convergedA / 2)
    newVolatility
  }

  def deviationGivenVolatility(deviationA: Double, volatility: Double): Double = sqrt(pow(deviationA, 2) + pow(volatility, 2))

  def newDeviation(deviationA: Double, variance: Double) = {
    1 / sqrt(1 / pow(deviationA, 2) + 1 / variance)
  }

  def newRatingValue(ratingA: Double, deviationA: Double, g: Double, score: Double, E: Double) = ratingA + pow(deviationA, 2) * g * (score - E)

  def newRating(ratingA: Rating, ratingB: Rating, score: Double, tau: Double, timestamp: Date, discountDurationInDays: Int): Rating = {
    val discountPeriods = discountPeriod(ratingA.timestamp, timestamp, discountDurationInDays)
    val deviationAfterEmptyPeriods =
      (1 to discountPeriods.toInt).foldLeft(ratingA.deviation)((latestDeviation, period) =>
        deviationGivenVolatility(latestDeviation, ratingA.volatility))
    
    val g = GenericGlicko2Rating.g(ratingB.deviation)
    val E = GenericGlicko2Rating.E(ratingA.rating, ratingB.rating, ratingB.deviation)
    val variance = GenericGlicko2Rating.variance(g, E)
    val delta = GenericGlicko2Rating.delta(variance, g, score, E)
    val newVolatilityValue = newVolatility(ratingA.deviation, ratingA.volatility, variance, delta, tau)
    val preRatingPeriodDeviation = GenericGlicko2Rating.deviationGivenVolatility(deviationAfterEmptyPeriods, newVolatilityValue)

    val newDeviation = GenericGlicko2Rating.newDeviation(preRatingPeriodDeviation, variance)
    val newRatingValue = GenericGlicko2Rating.newRatingValue(ratingA.rating, newDeviation, g, score, E)
    Rating(newRatingValue, newDeviation, newVolatilityValue, timestamp)
  }

  def discountPeriod(previousTimestamp: Date, currentTimestamp: Date, discountDurationInDays: Int): Long =
    (currentTimestamp.getTime() - previousTimestamp.getTime()) / 1000 / 3600 / 24 / discountDurationInDays

}
class GenericGlicko2Rating(initialRating: Double = 0, initialDeviation: Double = 350d / 173.7178, initialVolatility: Double = 0.06, tau: Double = 0.5, discountDurationInDays: Int) extends Glicko2Rating {

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

    val newRatingAOnServe = newRating(currRatingA.ratingOnServe, currRatingB.ratingOnReturn, result.score, tau, result.timestamp, discountDurationInDays)
    val newRatingBOnReturn = newRating(currRatingB.ratingOnReturn, currRatingA.ratingOnServe, 1 - result.score, tau, result.timestamp, discountDurationInDays)

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
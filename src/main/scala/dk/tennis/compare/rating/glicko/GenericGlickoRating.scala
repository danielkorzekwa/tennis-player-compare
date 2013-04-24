package dk.tennis.compare.rating.glicko

import GlickoRating._
import scala.math._
import java.util.Date
import org.joda.time._
import GenericGlickoRating._

/**Implementation of Glicko rating: http://www.glicko.net/glicko/glicko.pdf*/

/**
 * @param initialRating
 * @param initialDeviation
 * @param discountConstant constant that governs the increase in uncertainty over time
 */

object GenericGlickoRating {
  /**Glicko functions.*/

  def q = log(10) / 400

  def g(rd: Double) = 1 / (sqrt(1 + 3 * pow(q, 2) * pow(rd, 2) / pow(Pi, 2)))

  def expectedScore(ratingA: Double, ratingB: Double, deviationB: Double): Double = 1 / (1 + pow(10, -g(deviationB) * (ratingA - ratingB) / 400))

  def dSquare(ratingA: Double, ratingB: Double, deviationB: Double): Double =
    pow(pow(q, 2) * pow(g(deviationB), 2) * expectedScore(ratingA, ratingB, deviationB) * (1 - expectedScore(ratingA, ratingB, deviationB)), -1)

  def newRating(ratingA: Rating, ratingB: Rating, scoreAagainstB: Double): Double =
    ratingA.rating +
      q / (1 / pow(ratingA.deviation, 2) + 1 / dSquare(ratingA.rating, ratingB.rating, ratingB.deviation)) *
      g(ratingB.deviation) *
      (scoreAagainstB - expectedScore(ratingA.rating, ratingB.rating, ratingB.deviation))

  def newDeviation(ratingA: Rating, ratingB: Rating) = sqrt(pow(1 / pow(ratingA.deviation, 2) + 1 / dSquare(ratingA.rating, ratingB.rating, ratingB.deviation), -1))

  def newRating(ratingA: Rating, ratingB: Rating, score: Double, timestamp: Date): Rating = {
    val newRatingAOnServe: Double = GenericGlickoRating.newRating(ratingA, ratingB, score)
    val newDeviationAOnServe = newDeviation(ratingA, ratingB)
    val rating = Rating(newRatingAOnServe, newDeviationAOnServe, timestamp)
    rating
  }

  def discountRating(rating: Rating, currentTimestamp: Date, initialDeviation: Double, discountConstant: Double, discountDurationInDays: Int): Rating = {
    val t = discountPeriod(rating.timestamp, currentTimestamp, discountDurationInDays)
    val discountedDeviation = discountDeviation(rating.deviation, initialDeviation, discountConstant, t)
    rating.copy(deviation = discountedDeviation)
  }

  /**
   * @param deviation
   * @param t the number of rating periods since last competition (e.g., if the player competed in the most recent rating period, t = 1)
   */
  def discountDeviation(deviation: Double, initialDeviation: Double, discountConstant: Double, t: Long): Double = sqrt(pow(deviation, 2) + pow(discountConstant, 2) * t).min(initialDeviation)

  /**
   * @param deviation
   * @param t the number of rating periods since last competition (e.g., if the player competed in the most recent rating period, t = 1)
   */
  def discountConstant(deviation: Double, initialDeviation: Double, t: Double): Double = sqrt((pow(initialDeviation, 2) - pow(deviation, 2)) / t)

  def discountPeriod(previousTimestamp: Date, currentTimestamp: Date, discountDurationInDays: Int): Long =
    (currentTimestamp.getTime() - previousTimestamp.getTime()) / 1000 / 3600 / 24 / discountDurationInDays

}
class GenericGlickoRating(initialRating: Double = 1500, initialDeviation: Double = 100, discountConstant: Double = 13.587, discountDurationInDays: Int = 7) extends GlickoRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result]): Map[String, Rating] = {

    def updateRatings(ratings: Map[String, Rating], result: Result): Map[String, Rating] = {
      val currentRatingA: Rating = ratings.getOrElse(result.playerA, Rating(initialRating, initialDeviation, result.timestamp))
      val discountedCurrentRatingA = discountRating(currentRatingA, result.timestamp, initialDeviation, discountConstant, discountDurationInDays)
      val currentRatingB: Rating = ratings.getOrElse(result.playerB, Rating(initialRating, initialDeviation, result.timestamp))
      val discountCurrentRatingB = discountRating(currentRatingB, result.timestamp, initialDeviation, discountConstant, discountDurationInDays)

      val newRatingA = newRating(discountedCurrentRatingA, discountCurrentRatingB, result.score, result.timestamp)
      val newRatingB = newRating(discountCurrentRatingB, discountedCurrentRatingA, 1 - result.score, result.timestamp)

      val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)
      newRatings
    }

    /**Map[player,rating]*/
    val ratings = results.foldLeft(Map[String, Rating]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  /**
   * @return Map[player,Tuple2[rating on serve, rating on return]
   */
  def calcServeReturnRatings(results: List[Result]): Map[String, Tuple2[Rating, Rating]] = {

    /**Tuple2[rating on serve, rating on return]*/
    def initRating(timestamp: Date) = Tuple2(Rating(initialRating, initialDeviation, timestamp), Rating(initialRating, initialDeviation, timestamp))

    def updateRatings(ratings: Map[String, Tuple2[Rating, Rating]], result: Result): Map[String, Tuple2[Rating, Rating]] = {
      val (currRatingAOnServe, currRatingAOnReturn): Tuple2[Rating, Rating] = ratings.getOrElse(result.playerA, initRating(result.timestamp))
      val discountedRatingAOnServe = discountRating(currRatingAOnServe, result.timestamp, initialDeviation, discountConstant, discountDurationInDays)

      val (currRatingBOnServe, currRatingBOnReturn): Tuple2[Rating, Rating] = ratings.getOrElse(result.playerB, initRating(result.timestamp))
      val discountedRatingBOnReturn = discountRating(currRatingBOnReturn, result.timestamp, initialDeviation, discountConstant, discountDurationInDays)

      val newRatingAOnServe = newRating(discountedRatingAOnServe, discountedRatingBOnReturn, result.score, result.timestamp)
      val newRatingBOnReturn = newRating(discountedRatingBOnReturn, discountedRatingAOnServe, 1 - result.score, result.timestamp)

      val newRatings = ratings + (result.playerA -> (newRatingAOnServe, currRatingAOnReturn), result.playerB -> (currRatingBOnServe, newRatingBOnReturn))
      newRatings
    }

    /**Map[player,Tuple2[rating on serve, rating on return]*/
    val ratings = results.foldLeft(Map[String, Tuple2[Rating, Rating]]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

}
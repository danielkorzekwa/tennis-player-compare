package dk.tennis.compare.glicko

import GlickoRating._
import Math._

/**Implementation of Glicko rating: http://www.glicko.net/glicko/glicko.pdf*/
class GenericGlickoRating(initialRating: Double = 1500, initialDeviation: Double = 350) extends GlickoRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result]): Map[String, Rating] = {

    def updateRatings(ratings: Map[String, Rating], result: Result): Map[String, Rating] = {
      val currentRatingA: Rating = ratings.getOrElse(result.playerA, Rating(initialRating, initialDeviation))
      val currentRatingB: Rating = ratings.getOrElse(result.playerB, Rating(initialRating, initialDeviation))

      val newRatingA = newRating(currentRatingA, currentRatingB, result.score)
      val newDeviationA = newDeviation(currentRatingA, currentRatingB)
      val newRatingB = newRating(currentRatingB, currentRatingA, 1 - result.score)
      val newDeviationB = newDeviation(currentRatingB, currentRatingA)

      val newRatings = ratings + (result.playerA -> Rating(newRatingA, newDeviationA), result.playerB -> Rating(newRatingB, newDeviationB))
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
    def initRating = Tuple2(Rating(initialRating, initialDeviation), Rating(initialRating, initialDeviation))

    def updateRatings(ratings: Map[String, Tuple2[Rating, Rating]], result: Result): Map[String, Tuple2[Rating, Rating]] = {
      val currentRatingA: Tuple2[Rating, Rating] = ratings.getOrElse(result.playerA, initRating)
      val currRatingAOnServe = currentRatingA._1
      val currentRatingB: Tuple2[Rating, Rating] = ratings.getOrElse(result.playerB, initRating)
      val currRatingBOnReturn = currentRatingB._2

      val newRatingAOnServe = newRating(currRatingAOnServe, currRatingBOnReturn, result.score)
      val newDeviationAOnServe = newDeviation(currRatingAOnServe, currRatingBOnReturn)
      val newRatingA = Rating(newRatingAOnServe, newDeviationAOnServe)

      val newRatingOnReturnB = newRating(currRatingBOnReturn, currRatingAOnServe, 1 - result.score)
      val newDeviationBOnReturn = newDeviation(currRatingBOnReturn, currRatingAOnServe)
      val newRatingB = Rating(newRatingOnReturnB, newDeviationBOnReturn)

      val newRatings = ratings + (result.playerA -> (newRatingA, currentRatingA._2), result.playerB -> (currentRatingB._1, newRatingB))
      newRatings
    }

    /**Map[player,Tuple2[rating on serve, rating on return]*/
    val ratings = results.foldLeft(Map[String, Tuple2[Rating, Rating]]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  /**Glicko functions.*/

  def q = log(10) / 400

  def g(rd: Double) = 1 / (sqrt(1 + 3 * pow(q, 2) * pow(rd, 2) / pow(Pi, 2)))

  def expectedScore(ratingA: Double, ratingB: Double, deviationB: Double): Double = 1 / (1 + pow(10, -g(deviationB) * (ratingA - ratingB) / 400))

  def dSquare(ratingA: Double, ratingB: Double, deviationB: Double): Double =
    pow(pow(q, 2) * pow(g(deviationB), 2) * expectedScore(ratingA, ratingB, deviationB) * (1 - expectedScore(ratingA, ratingB, deviationB)), -1)

  def newRating(ratingA: Rating, ratingB: Rating, scoreAagainstB: Double) =
    ratingA.rating +
      q / (1 / pow(ratingA.deviation, 2) + 1 / dSquare(ratingA.rating, ratingB.rating, ratingB.deviation)) *
      g(ratingB.deviation) *
      (scoreAagainstB - expectedScore(ratingA.rating, ratingB.rating, ratingB.deviation))

  def newDeviation(ratingA: Rating, ratingB: Rating) = sqrt(pow(1 / pow(ratingA.deviation, 2) + 1 / dSquare(ratingA.rating, ratingB.rating, ratingB.deviation), -1))
}
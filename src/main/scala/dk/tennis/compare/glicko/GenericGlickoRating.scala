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

      val newRatingA = newRating(currentRatingA.rating, currentRatingA.deviation, currentRatingB.rating, currentRatingB.deviation, result.score)
      val newDeviationA = newDeviation(currentRatingA.rating, currentRatingA.deviation, currentRatingB.rating, currentRatingB.deviation)
      val newRatingB = newRating(currentRatingB.rating, currentRatingB.deviation, currentRatingA.rating, currentRatingA.deviation, 1 - result.score)
      val newDeviationB = newDeviation(currentRatingB.rating, currentRatingB.deviation, currentRatingA.rating, currentRatingA.deviation)

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
  def calcServeReturnRatings(results: List[Result]): Map[String, Tuple2[Rating, Rating]] = throw new UnsupportedOperationException("Not implemented yet.")

  /**Glicko functions.*/

  def q = log(10) / 400

  def g(rd: Double) = 1 / (sqrt(1 + 3 * pow(q, 2) * pow(rd, 2) / pow(Pi, 2)))

  def expectedScore(ratingA: Double, ratingB: Double, deviationB: Double): Double = 1 / (1 + pow(10, -g(deviationB) * (ratingA - ratingB) / 400))

  def dSquare(ratingA: Double, ratingB: Double, deviationB: Double): Double =
    pow(pow(q, 2) * pow(g(deviationB), 2) * expectedScore(ratingA, ratingB, deviationB) * (1 - expectedScore(ratingA, ratingB, deviationB)), -1)

  def newRating(ratingA: Double, deviationA: Double, ratingB: Double, deviationB: Double, scoreAagainstB: Double) =
    ratingA + q / (1 / pow(deviationA, 2) + 1 / dSquare(ratingA, ratingB, deviationB)) * g(deviationB) * (scoreAagainstB - expectedScore(ratingA, ratingB, deviationB))

  def newDeviation(ratingA: Double, deviationA: Double, ratingB: Double, deviationB: Double) = sqrt(pow(1 / pow(deviationA, 2) + 1 / dSquare(ratingA, ratingB, deviationB), -1))
}
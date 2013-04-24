package dk.tennis.compare.rating.elo
import scala.math._
import EloRating._
import scala.collection.immutable.Map

class GenericEloRating(kFactor: Double = 32, initialRating: Double = 1000) extends EloRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result]): Map[String, Double] = {

    def updateRatings(ratings: Map[String, Double], result: Result): Map[String, Double] = {
      val currentRatingA: Double = ratings.getOrElse(result.playerA, initialRating)
      val currentRatingB: Double = ratings.getOrElse(result.playerB, initialRating)

      val newRatingA = newRating(currentRatingA, currentRatingB, result.score)
      val newRatingB = newRating(currentRatingB, currentRatingA, 1 - result.score)

      val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)
      newRatings
    }

    /**Map[player,rating]*/
    val ratings = results.foldLeft(Map[String, Double]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  /**
   * @return Map[player,Tuple2[rating on serve, rating on return]
   */
  def calcServeReturnRatings(results: List[Result]): Map[String, Tuple2[Double, Double]] = {

    def updateRatings(ratings: Map[String, Tuple2[Double, Double]], result: Result): Map[String, Tuple2[Double, Double]] = {
      val currRatingA = ratings.getOrElse(result.playerA, Tuple2(initialRating, initialRating))
      val currRatingAOnServe = currRatingA._1
      val currRatingB = ratings.getOrElse(result.playerB, Tuple2(initialRating, initialRating))
      val currRatingBOnReturn = currRatingB._2

      val newRatingAOnServe = newRating(currRatingAOnServe, currRatingBOnReturn, result.score)
      val newRatingOnReturnB = newRating(currRatingBOnReturn, currRatingAOnServe, 1 - result.score)

      val newRatings = ratings + (result.playerA -> (newRatingAOnServe, currRatingA._2), result.playerB -> (currRatingB._1, newRatingOnReturnB))
      newRatings
    }

    val ratings = results.foldLeft(Map[String, Tuple2[Double, Double]]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  /**Elo functions.*/
  def calcExpectedScore(ratingA: Double, ratingB: Double): Double = 1 / (1 + pow(10, (ratingB - ratingA) / 400))
  def newRating(currentRatingA: Double, currentRatingB: Double, actualScore: Double): Double =
    currentRatingA + kFactor * (actualScore - calcExpectedScore(currentRatingA, currentRatingB))
}
package dk.tennis.compare.elo
import scala.Math._
import EloRating._
import scala.collection.immutable.Map

object GenericEloRating extends EloRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result],kFactor:Double=32): Map[String, Double] = {

    def updateRatings(ratings: Map[String, Double], result: Result): Map[String, Double] = {
      val currentRatingA: Double = ratings.getOrElse(result.playerA, 1000)
      val currentRatingB: Double = ratings.getOrElse(result.playerB, 1000)
      
      val actualScoreA = result.gamesWon.toDouble/result.gamesPlayed
      val actualScoreB = 1 - actualScoreA
      val expectedScoreA =  calcExpectedScore(currentRatingA, currentRatingB)
      val expectedScoreB =  calcExpectedScore(currentRatingB, currentRatingA)
     
      val newRatingA = currentRatingA + kFactor * (actualScoreA - expectedScoreA)
      val newRatingB = currentRatingB + kFactor * (actualScoreB - expectedScoreB)

      val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)
      newRatings
    }

    /**Map[player,rating]*/
    val ratings = results.foldLeft(Map[String, Double]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  def calcExpectedScore(ratingA: Double, ratingB: Double): Double = 1 / (1 + pow(10, (ratingB - ratingA) / 400))
}
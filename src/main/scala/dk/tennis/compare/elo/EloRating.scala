package dk.tennis.compare.elo

import EloRating._

/**Implementation of Elo rating: http://en.wikipedia.org/wiki/Elo_rating_system.
 * 
 */
object EloRating {
  /**
   * @param playerA
   * @param playerB
   * @param gamesWon Number of games won by player A.
   * @param gamesPlayed Number of all games played between players A and B.
   */
  case class Result(playerA:String, playerB:String,gamesWon:Int, gamesPlayed:Int)
}
trait EloRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results:List[Result]):Map[String,Double]
  
  def calcExpectedScore(ratingA:Double,ratingB:Double):Double
}
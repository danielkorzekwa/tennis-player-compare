package dk.tennis.compare.elo

import EloRating._

/**Implementation of Elo rating: http://en.wikipedia.org/wiki/Elo_rating_system.
 * 
 */
object EloRating {
  /**
   * @param playerA
   * @param playerB
   * @param score Between 0 and 1, 0 - player A lost, 1 player A won, 0.75 - 75% chances of winning by player A
   */
  case class Result(playerA:String, playerB:String,score:Double)
}
trait EloRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result]): Map[String, Double]
  
  /**
   * @return Map[player,Tuple2[rating on serve, rating on return]
   */
  def calcServeReturnRatings(results: List[Result]):Map[String, Tuple2[Double,Double]]
 
}
package dk.tennis.compare.glicko

import GlickoRating._
import java.util.Date

/**Implementation of Glicko rating: http://www.glicko.net/glicko/glicko.pdf*/
object GlickoRating {
  /**
   * @param playerA
   * @param playerB
   * @param score Between 0 and 1, 0 - player A lost, 1 player A won, 0.75 - 75% chances of winning by player A
   * @param timestamp
   */
  case class Result(playerA: String, playerB: String, score: Double, timestamp: Date)

  case class Rating(rating: Double, deviation: Double, timestamp: Date)
}
trait GlickoRating {

  /**
   * @return Map[player,rating]
   */
  def calcRatings(results: List[Result]): Map[String, Rating]

  /**
   * @return Map[player,Tuple2[rating on serve, rating on return]
   */
  def calcServeReturnRatings(results: List[Result]): Map[String, Tuple2[Rating, Rating]]

}
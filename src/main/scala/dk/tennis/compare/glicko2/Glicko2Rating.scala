package dk.tennis.compare.glicko2

import java.util.Date
import Glicko2Rating._

/**Glicko 2 ratings - http://glicko.net/glicko/glicko2.pdf*/
object Glicko2Rating {
  /**
   * @param playerA
   * @param playerB
   * @param score Between 0 and 1, 0 - player A lost, 1 player A won, 0.75 - 75% chances of winning by player A
   * @param timestamp
   */
  case class Result(playerA: String, playerB: String, score: Double, timestamp: Date)

  case class PlayerRating(ratingOnServe: Rating, ratingOnReturn: Rating)
  case class Rating(rating: Double, deviation: Double, volatility:Double, timestamp: Date)
}

trait Glicko2Rating {

  /**
   * @return Map[playerName, playerRating]
   */
  def calcServeReturnRatings(results: List[Result]): Map[String, PlayerRating]
  
  /**Processes tennis match event and updates internal player ratings.
   * 
   */
  def sendResult(result:Result)
  
  /**
   * @return Map [player name, player rating],
   */
  def getRatings(): Map[String,PlayerRating]

}
package  dk.tennis.compare.glicko2

import java.util.Date
import  dk.tennis.compare.glicko._
import Glicko2Rating._
import dk.atp.api.domain.SurfaceEnum._
import  scala.collection._

/**Returns Glicko2 ratings for a given time stamp.*/
trait Glicko2RatingsLoader {

  /**
   * @return  [playerName,playerRating]
   */
  def ratings(timestamp:Date,surface:SurfaceEnum):Map[String, PlayerRating]
}
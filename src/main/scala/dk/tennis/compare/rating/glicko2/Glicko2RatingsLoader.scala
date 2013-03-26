package  dk.tennis.compare.rating.glicko2

import java.util.Date

import scala.collection.Map

import Glicko2Rating.PlayerRating
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum

/**Returns Glicko2 ratings for a given time stamp.*/
trait Glicko2RatingsLoader {

  /**
   * @return  [playerName,playerRating]
   */
  def ratings(timestamp:Date,surface:SurfaceEnum):Map[String, PlayerRating]
}
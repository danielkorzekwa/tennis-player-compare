package  dk.tennis.compare.rating.glicko

import java.util.Date

import scala.collection.Map

import GlickoRating.Rating
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum

/**Returns Glicko ratings for a given time stamp.*/
trait GlickoRatingsLoader {

  /**
   * @return  [playerName,[ratingOnServe,ratingOnReturn]]
   */
  def ratings(timestamp:Date,surface:SurfaceEnum):Map[String, Tuple2[Rating, Rating]]
}
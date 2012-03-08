package  dk.tennis.compare.glicko

import java.util.Date
import  dk.tennis.compare.glicko._
import GlickoRating._
import dk.atp.api.domain.SurfaceEnum._
import  scala.collection._

/**Returns Glicko ratings for a given time stamp.*/
trait GlickoRatingsLoader {

  /**
   * @return  [playerName,[ratingOnServe,ratingOnReturn]]
   */
  def ratings(timestamp:Date,surface:SurfaceEnum):Map[String, Tuple2[Rating, Rating]]
}
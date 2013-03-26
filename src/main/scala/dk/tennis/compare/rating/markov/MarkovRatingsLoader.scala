package dk.tennis.compare.rating.markov

import dk.atp.api.domain.SurfaceEnum
import dk.tennis.compare.rating.markov.MarkovRating.PlayerRating
import java.util.Date
import dk.atp.api.domain.SurfaceEnum._
import scala.collection._

/**
 * Returns tennis ratings at a given time.
 *
 */
trait MarkovRatingsLoader {

  /**
   * Returns tennis ratings at a given time.
   *
   * @return  [playerName,playerRating]
   */
  def ratings(timestamp:Date,surface:SurfaceEnum):Map[String, PlayerRating]
  
  def getMarkovRating():MarkovRating
}
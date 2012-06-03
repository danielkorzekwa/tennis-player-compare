package dk.tennis.compare.markov

import java.util.Date
import dk.tennis.compare.markov._
import MarkovRating._
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
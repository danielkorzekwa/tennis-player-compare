package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._

object MatchLogLik {

  /**
   * Returns match log likelihood given match outcome (winner or loser).
   */
  def logLik(p1MatchProb:Double, p1Won:Boolean): Double = {
     val matchLogLik = if (p1Won) log(p1MatchProb) else log1p(-p1MatchProb)
     matchLogLik
  }

}
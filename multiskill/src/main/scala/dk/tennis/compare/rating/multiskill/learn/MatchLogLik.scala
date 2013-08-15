package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import scala.math._

object MatchLogLik {

  /**
   * Returns match log likelihood given match outcome (winner or loser).
   */
  def logLik(p1MatchProb:Double, p1Won:Boolean): Double = {
     val matchLogLik = if (p1Won) log(p1MatchProb) else log1p(-p1MatchProb)
     matchLogLik
  }

  /**
   * Returns match point-by-point log likelihood given the sequence of point results for a tennis match.
   */
  def logLikByPoint(p1OnServePointProb: Double, p2OnServePointProb: Double, result: MatchResult): Double = {
      val loglik = result.pointResults.foldLeft(0d) { (totalLogLik, point) =>

      val pointProbOnServe = if (result.player1.equals(point.playerOnServe)) p1OnServePointProb else p2OnServePointProb
      val pointLoglik = if (point.playerOnServeWin) log(pointProbOnServe) else log1p(-pointProbOnServe)
      totalLogLik + pointLoglik
    }
      
      loglik
  }
  

}
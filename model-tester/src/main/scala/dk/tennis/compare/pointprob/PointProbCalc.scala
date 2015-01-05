package dk.tennis.compare.pointprob

import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

/**
 *  Derives the probability of winning a point on serve by a tennis player given the probability of winning a match.
 *
 * @author Daniel Korzekwa
 */
trait PointProbCalc {

  /**
   * Derives the probability of winning a point on serve by a tennis player given the probability of winning a match.
   *
   * @param matchProb Probability of winning a match by player1 against player2
   * @param matchType Three or five sets match
   *
   * @return The probability of winning a point on serve by player 1. The probability of winning a point on serve by player 2 = 1 - player1PointProbOnServe
   */
  def calcPointProb(matchProb: Double, matchType: MatchTypeEnum): Double
}
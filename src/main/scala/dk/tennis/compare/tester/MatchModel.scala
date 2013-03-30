package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite

trait MatchModel {

  /**
   * Returns the probability of winning the tennis game by player A.
   */
  def matchProb(m: MatchComposite):Option[Double]
  
  def addMatchResult(m:MatchComposite)
}
package dk.tennis.compare.rating.multiskill.model.pointmodel

import dk.bayes.math.gaussian.Gaussian

trait PointModel {

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner.
   *
   * @return Posterior for [P1Marginal,P2Marginal]
   */
  def skillMarginals(player1Skill: Gaussian, player2Skill: Gaussian, p1Wins: Boolean): Tuple2[Gaussian, Gaussian]

  /**
   * Returns the probability of winning the point by player 1.
   */
  def pointProb(player1Skill: Gaussian, player2Skill: Gaussian): Double
}
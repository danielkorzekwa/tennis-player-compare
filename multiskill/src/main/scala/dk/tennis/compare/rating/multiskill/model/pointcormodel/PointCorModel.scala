package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.bayes.math.gaussian.CanonicalGaussian

trait PointCorModel {

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner or loser.
   *
   * @param directSkills Skills for players 1 and 2
   * @param p1Wins
   * @return Posterior for skills of player 1 and 2
   */
  def skillMarginals(directSkills: CanonicalGaussian, p1Wins: Boolean): CanonicalGaussian

  /**
   * @param directSkills Skills for players 1 and 2
   *
   * Returns the probability of winning the point by player 1.
   */
  def pointProb(directSkills: CanonicalGaussian): Double
}
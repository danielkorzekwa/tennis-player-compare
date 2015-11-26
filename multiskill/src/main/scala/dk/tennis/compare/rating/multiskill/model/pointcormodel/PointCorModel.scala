package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian


trait PointCorModel {

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner or loser.
   *
   * @param directSkills Skills for players 1 and 2
   * @param p1Wins
   * @return Posterior for skills of player 1 and 2
   */
  def skillMarginals(directSkills: DenseCanonicalGaussian, p1Wins: Boolean): DenseCanonicalGaussian

  /**
   * @param directSkills Skills for players 1 and 2
   *
   * Returns the probability of winning the point by player 1.
   */
  def pointProb(directSkills: DenseCanonicalGaussian): Double
}
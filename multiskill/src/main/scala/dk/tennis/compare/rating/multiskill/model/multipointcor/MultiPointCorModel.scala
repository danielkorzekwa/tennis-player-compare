package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.bayes.math.gaussian.CanonicalGaussian

trait MultiPointCorModel {

  /**
   * Returns posterior marginals for player1 and player2 skills given player1 wins p out of n tennis points.
   *
   * @param directSkills Skills for players 1 and 2
   * @param pointsWon
   * @param allPoints
   * @return Posterior for skills for player 1 and 2
   */
  def skillMarginals(directSkills: CanonicalGaussian, pointsWon: Int, allPoints: Int, maxIter: Int = 100, threshold: Double): CanonicalGaussian
}
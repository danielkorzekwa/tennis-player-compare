package dk.tennis.compare.rating.multiskill.model.multipoint

import dk.bayes.math.gaussian.Gaussian

/**
 * @author Daniel Korzekwa
 */
trait MultiPointModel {

  /**
   * Returns posterior marginals for player1 and player2 skills given player1 wins p out of n tennis points.
   *
   * @return Posterior for [P1Marginal,P2Marginal]
   */
  def skillMarginals(player1Skill: Gaussian, player2Skill: Gaussian, pointsWon: Int, allPoints: Int,maxIter:Int, threshold: Double ): Tuple3[Gaussian,Gaussian,PointsFactorGraph]
}
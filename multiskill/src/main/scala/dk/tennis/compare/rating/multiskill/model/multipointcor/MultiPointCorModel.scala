package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.multipoint.PointsFactorGraph

trait MultiPointCorModel {

  /**
   * Returns posterior marginals for player1 and player2 skills given player1 wins p out of n tennis points.
   *
   * @param player1Skill
   * @param player2Skill
   * @param pointsWon
   * @param allPoints
   * @param skillCovariance Covariance between skills for players 1 and 2
   * @return Posterior for [P1Marginal,P2Marginal,skill's covariance]
   */
  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, pointsWon: Int, allPoints: Int, skillCovariance: Double): Tuple3[PlayerSkill, PlayerSkill,Double]
}
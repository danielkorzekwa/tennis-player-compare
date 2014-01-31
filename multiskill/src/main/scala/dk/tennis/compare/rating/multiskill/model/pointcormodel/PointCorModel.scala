package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

trait PointCorModel {

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner or loser.
   *
   * @param player1Skill
   * @param player2Skill
   * @param p1Wins
   * @param skillCovariance Covariance between skills for players 1 and 2
   * @return Posterior for [P1Marginal,P2Marginal,skill covariance]
   */
  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, p1Wins: Boolean, skillCovariance: Double): Tuple3[PlayerSkill, PlayerSkill, Double]

  /**
   * @param player1Skill
   * @param player2Skill
   * @param skillCovariance Covariance between skills for players 1 and 2
   *
   * Returns the probability of winning the point by player 1.
   */
  def pointProb(player1Skill: PlayerSkill, player2Skill: PlayerSkill, skillCovariance: Double): Double
}
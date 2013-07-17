package dk.tennis.compare.rating.multiskill.pointmodel

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

trait PointModel {

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner.
   *
   * @return Posterior for [P1Marginal,P2Marginal]
   */
  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, p1Wins: Boolean): Tuple2[PlayerSkill, PlayerSkill]

  /**
   * Returns the probability of winning the point by player 1.
   */
  def pointProb(player1Skill: PlayerSkill, player2Skill: PlayerSkill): Double
}
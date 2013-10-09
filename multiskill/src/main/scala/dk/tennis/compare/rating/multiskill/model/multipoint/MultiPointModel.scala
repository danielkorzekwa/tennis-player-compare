package dk.tennis.compare.rating.multiskill.model.multipoint

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

/**
 * @author Daniel Korzekwa
 */
trait MultiPointModel {

  /**
   * Returns posterior marginals for player1 and player2 skills given player1 wins p out of n tennis points.
   *
   * @return Posterior for [P1Marginal,P2Marginal]
   */
  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, pointsWon: Int, allPoints: Int): Tuple2[PlayerSkill, PlayerSkill]
}
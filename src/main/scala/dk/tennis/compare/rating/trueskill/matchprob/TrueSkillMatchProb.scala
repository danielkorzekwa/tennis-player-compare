package dk.tennis.compare.rating.trueskill.matchprob

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating


trait TrueSkillMatchProb {

  /**
   * Returns the probability of winning the game by player 1
   *
   * @param player1Skill
   * @param player2Skill
   * @param perfVariance The player's performance variances. Tuple2[player1 variance, player2 variance]
   */
  def matchProb(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating, perfVariance: Tuple2[Double, Double]): Double
}
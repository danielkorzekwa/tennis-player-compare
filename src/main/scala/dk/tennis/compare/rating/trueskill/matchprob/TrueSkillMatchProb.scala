package dk.tennis.compare.rating.trueskill.matchprob

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

trait TrueSkillMatchProb {

  /**
   * Returns the probability of winning the game by player 1
   */
  def matchProb(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating): Double
}
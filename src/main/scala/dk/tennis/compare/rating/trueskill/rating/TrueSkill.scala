package dk.tennis.compare.rating.trueskill.rating

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

trait TrueSkill {

  def addResult(player1: String, player2: String, player1Win: Boolean)

  /**
   * @return Map[playerName,playerSkill]
   */
  def getRatings(): Map[String, TrueSkillRating]

}
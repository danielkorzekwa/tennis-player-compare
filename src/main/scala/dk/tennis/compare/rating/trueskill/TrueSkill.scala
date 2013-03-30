package dk.tennis.compare.rating.trueskill

import TrueSkill._

trait TrueSkill {

  def addResult(player1: String, player2: String, player1Win: Boolean)

  /**
   * @return Map[playerName,playerSkill]
   */
  def getRatings(): Map[String, TrueSkillRating]

  /**
   * Returns the probability of winning the game by player 1
   */
  def winnerProb(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating): Double
}

object TrueSkill {

  case class TrueSkillRating(skill: Double, volatility: Double)
}
package dk.tennis.compare.rating.trueskill.ratingdbn

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.Result

trait TrueSkillDbn {

  def addResult(result: Result)

  /**
   * @return Map[playerName,playerSkill]
   */
  def calcRatings(): Map[String, TrueSkillRating]
}
package dk.tennis.compare.rating.trueskill.rating

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.Result

trait TrueSkill {

  /**
   * @param result
   * @param perfVariance The player's performance variances. Tuple2[player1 variance, player2 variance]
   */
  def addResult(result: Result, perfVariance: Tuple2[Double, Double])

  /**
   * @return Map[playerName,playerSkill]
   */
  def getRatings(): Map[String, TrueSkillRating]

}
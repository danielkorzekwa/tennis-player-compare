package dk.tennis.compare.rating.trueskill

import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

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
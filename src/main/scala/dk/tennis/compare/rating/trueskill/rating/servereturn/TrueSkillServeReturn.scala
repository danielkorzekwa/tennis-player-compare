package dk.tennis.compare.rating.trueskill.rating.servereturn

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.Result

trait TrueSkillServeReturn {

  /**
   * @param result
   * @param perfVariance The player's performance variances. Tuple2[player1 variance, player2 variance]
   */
  def addResult(result: Result, perfVariance: Tuple2[Double, Double])

  /**
   * @return Map[playerName,Tuple2[playerSkillServe,playerSkillReturn]]
   */
  def getRatings(): Map[String, Tuple2[TrueSkillRating,TrueSkillRating]]

}
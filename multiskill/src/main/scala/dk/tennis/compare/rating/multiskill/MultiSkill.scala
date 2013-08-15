package dk.tennis.compare.rating.multiskill

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult

trait MultiSkill {

  def processTennisMatch(matchResult: MatchResult)

  /**
   * @return Map[playerName,player skills]
   */
  def getSkills(): Map[String, PlayerSkills]

  def getSkill(player:String):PlayerSkills
  
}
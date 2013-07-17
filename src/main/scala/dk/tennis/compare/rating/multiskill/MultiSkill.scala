package dk.tennis.compare.rating.multiskill

import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PointResult

trait MultiSkill {

  def processTennisMatch(player1:String,player2:String,pointResults: Seq[PointResult])

  /**
   * @return Map[playerName,player skills]
   */
  def getSkills(): Map[String, PlayerSkills]

}
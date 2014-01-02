package dk.tennis.compare.rating.multiskill.domain

import java.util.Date

case class PlayerSkills(player: String, timestamp: Date, skillOnServe: PlayerSkill, skillOnReturn: PlayerSkill) {

  def transition(newTimestamp: Date, transVarOnServe: Double, transVarOnReturn: Double): PlayerSkills = {

    val transSkills = this.copy(
      timestamp = newTimestamp,
      skillOnServe = skillOnServe.transition(transVarOnServe),
      skillOnReturn = skillOnReturn.transition(transVarOnReturn))

    transSkills
  }

  def transition(transVarOnServe: Double, transVarOnReturn: Double): PlayerSkills = {
    transition(timestamp, transVarOnServe, transVarOnReturn)
  }
}

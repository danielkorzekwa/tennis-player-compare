package dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import scala.util.Random
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score

case class SkillsGivenOpponent(
  skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]])

object SkillsGivenOpponent {

  def sample(scores: Seq[Score]): SkillsGivenOpponent = {
    val priorSkillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player1))
    val priorSkillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player2))
    val priorSkillsGivenOpponent = SkillsGivenOpponent(priorSkillsOnServeGivenOpponent, priorSkillsOnReturnGivenOpponent)

    priorSkillsGivenOpponent
  }
  /**
   * Returns Map[opponent name, player skills against opponent]
   */
  private def calcPriorSkillsGivenOpponent(playersGivenOpponent: Seq[Player]): Map[String, Seq[PlayerSkill]] = {

    val rand = new Random(444543)
    val allPlayers = playersGivenOpponent.map(p => p.playerName).distinct

    val skillsGivenOpponentMap = allPlayers.map { playerKey =>

      val skills = playersGivenOpponent.map(p => PlayerSkill(rand.nextDouble * 0.1, p.copy(opponentName = playerKey))).toSeq
      (playerKey, skills)
    }.toMap

    skillsGivenOpponentMap
  }

}
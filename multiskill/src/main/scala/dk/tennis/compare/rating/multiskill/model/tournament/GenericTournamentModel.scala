package dk.tennis.compare.rating.multiskill.model.tournament

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.collection._
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel

case class GenericTournamentModel(config: TournamentModelConfig) extends TournamentModel {

  def calcMatchSkills(initialPlayerSkills: immutable.Map[String, PlayerSkills], tennisMatches: Seq[MatchResult]): Seq[MatchSkills] = {
    /** Map[playerName,player skills]*/
    val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map() ++ initialPlayerSkills

    val matchSkills = tennisMatches.map { m =>

      val beforeGameP1Skills = skillsMap(m.player1)
      val beforeGameP2Skills = skillsMap(m.player2)

      //update player skills at the beginning of the next game
      val (afterGamePlayer1Skills, afterGamePlayer2Skills) = afterGameSkills(beforeGameP1Skills, beforeGameP2Skills, m)
      
      val beforeNextGameP1Skills = afterGamePlayer1Skills.transition(config.skillOnServeTransVariance, config.skillOnReturnTransVariance)
      val beforeNextGameP2Skills = afterGamePlayer2Skills.transition(config.skillOnServeTransVariance, config.skillOnReturnTransVariance)
      skillsMap += m.player1 -> beforeNextGameP1Skills
      skillsMap += m.player2 -> beforeNextGameP2Skills

      MatchSkills(m, beforeGameP1Skills, beforeGameP2Skills)

    }
    matchSkills
  }

  /**Returns (player1 skills, player2 skills) after observing the outcome of the game*/
  private def afterGameSkills(player1Skills: PlayerSkills, player2Skills: PlayerSkills, m: MatchResult): Tuple2[PlayerSkills, PlayerSkills] = {
    val multiPointModel = GenericMultiPointModel(config.pointPerfVarianceOnServe, config.pointPerfVarianceOnReturn)

    val (newP1SkillOnServe, newP2SkillOnReturn, _) =
      multiPointModel.skillMarginals(player1Skills.skillOnServe, player2Skills.skillOnReturn, m.p1Stats.servicePointsWon, m.p1Stats.servicePointsTotal)

    val (newP2SkillOnServe, newP1SkillOnReturn, _) =
      multiPointModel.skillMarginals(player2Skills.skillOnServe, player1Skills.skillOnReturn, m.p2Stats.servicePointsWon, m.p2Stats.servicePointsTotal)

    val newPlayer1Skills = PlayerSkills(player1Skills.player, player1Skills.timestamp, newP1SkillOnServe, newP1SkillOnReturn)
    val newPlayer2Skills = PlayerSkills(player2Skills.player, player2Skills.timestamp, newP2SkillOnServe, newP2SkillOnReturn)

    (newPlayer1Skills, newPlayer2Skills)
  }
}
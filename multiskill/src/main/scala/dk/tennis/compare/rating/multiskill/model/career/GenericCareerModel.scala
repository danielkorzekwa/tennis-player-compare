package dk.tennis.compare.rating.multiskill.model.career

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.collection._
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.career.tournament.GenericTournamentDBNModel
import com.typesafe.scalalogging.slf4j.Logging
case class GenericCareerModel(config: CareerModelConfig) extends CareerModel with Logging {

  def calcTournamentSkills(tournaments: Seq[TournamentResult]): Seq[TournamentSkills] = {

    val allMatches = tournaments.flatMap(t => t.matchResults)
    val allPlayers = allMatches.flatMap(m => List(m.player1, m.player2)).distinct

    val latestSkills: mutable.Map[String, PlayerSkills] = mutable.Map()
    val tSkills = tournaments.map(t => toTournamentSkills(allPlayers, t, latestSkills))
    tSkills
  }

  private def toTournamentSkills(allPlayers: Seq[String], t: TournamentResult, latestSkills: mutable.Map[String, PlayerSkills]): TournamentSkills = {

    val beforeTSkills: immutable.Map[String, PlayerSkills] = allPlayers.map { player =>
      val latestPlayerSkills = latestSkills.get(player)

      val beforeTPlayerSkills = latestPlayerSkills match {
        case None => PlayerSkills(player, t.tournamentTime, config.initialSkillsOnServe, config.initialSkillsOnReturn)
        case Some(latestPlayerSkills) => transition(latestPlayerSkills, t.tournamentTime)
      }
      (player -> beforeTPlayerSkills)
    }.toMap

    //update latest skills given tournament results
    val tournamentDBNModel = GenericTournamentDBNModel(beforeTSkills, t, config.pointPerfVarianceOnServe, config.pointPerfVarianceOnReturn)
    val epSummary = tournamentDBNModel.calibrate(200, iter => {})
    logger.info("Calibrating tournament dbn model: " + epSummary)

    val afterTSkills = tournamentDBNModel.getAfterTSkills()
    afterTSkills.foreach { case (player, playerSkills) => latestSkills += Tuple2(player, playerSkills) }

    TournamentSkills(t, beforeTSkills)
  }

  private def transition(playerSkills: PlayerSkills, tournamentTime: Date): PlayerSkills = {

    val timeDelta = (tournamentTime.getTime() - playerSkills.timestamp.getTime())
    val daysDelta = timeDelta / (1000 * 3600 * 24)
    val newTimestamp = new Date(playerSkills.timestamp.getTime() + timeDelta)

    val transSkills = playerSkills.transition(newTimestamp, daysDelta * config.skillOnServeTransVariance, daysDelta * config.skillOnReturnTransVariance)
    transSkills
  }

}
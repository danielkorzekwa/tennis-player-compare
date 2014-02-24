package dk.tennis.compare.rating.multiskill.model.career

import java.util.Date
import scala.collection.Seq
import scala.collection.Set
import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear.Matrix
import dk.bayes.math.gaussian.Linear.identity
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.career.tournament.GenericTournamentCorDBNModel

case class CorCareerModel(config: CareerModelConfig) extends Logging {

  def calcTournamentSkills(tournaments: Seq[TournamentResult]): Seq[TournamentSkillsCor] = {

    val allMatches = tournaments.flatMap(t => t.matchResults)
    val allPlayers = allMatches.flatMap(m => List(m.player1, m.player2)).distinct.toIndexedSeq
    logger.info("all players:" + allPlayers.size)

    //skills on serve + skills on return
    val skillsMean = Matrix(Array.fill(allPlayers.size)(config.initialSkillsOnServe.mean) ++ Array.fill(allPlayers.size)(config.initialSkillsOnReturn.mean))
    val skillsVar = Matrix.diag((Array.fill(allPlayers.size)(config.initialSkillsOnServe.variance) ++ Array.fill(allPlayers.size)(config.initialSkillsOnReturn.variance)): _*)
    var skillsGassian = CanonicalGaussian(skillsMean, skillsVar)

    var skillsTimestamp: Date = new Date(0)
    val activePlayers: mutable.Set[String] = new HashSet[String]()

    val tSkills: ListBuffer[TournamentSkillsCor] = new ListBuffer[TournamentSkillsCor]()

    tournaments.foreach { t =>

      val beforeTSkills = calcBeforeTournamentSkills(t, allPlayers, activePlayers, skillsTimestamp, skillsGassian, config.skillOnServeTransVariance, config.skillOnReturnTransVariance)

      val tournamentSkills = TournamentSkillsCor(t, allPlayers, beforeTSkills)
      tSkills += tournamentSkills

      val tournamentDBNModel = GenericTournamentCorDBNModel(allPlayers, beforeTSkills, t, config.pointPerfVarianceOnServe, config.pointPerfVarianceOnReturn)
      val epSummary = tournamentDBNModel.calibrate(10, iter => {})
      logger.info("Calibrating tournament dbn model: " + epSummary)

      skillsGassian = tournamentDBNModel.getAfterTSkills
      skillsTimestamp = t.tournamentTime

      t.players.foreach(p => activePlayers += p)

    }

    tSkills.toList
  }

  private def calcBeforeTournamentSkills(tournament: TournamentResult, allPlayers: Seq[String], activePlayers: Set[String], skillsTimestamp: Date, skills: CanonicalGaussian,
    skillOnServeTransVariance: Double, skillOnReturnTransVariance: Double): CanonicalGaussian = {
    val skillOnServeTransitionVector = allPlayers.map { p =>
      if (!activePlayers.contains(p)) 0
      else
        daysDelta(skillsTimestamp, tournament.tournamentTime) * skillOnServeTransVariance
    }
    val skillOnReturnTransitionVector = allPlayers.map { p =>
      if (!activePlayers.contains(p)) 0
      else
        daysDelta(skillsTimestamp, tournament.tournamentTime) * skillOnReturnTransVariance
    }

    val skillTransDiagMatrix = Matrix.diag((skillOnServeTransitionVector ++ skillOnReturnTransitionVector): _*)

    val (skillsMean, skillsVariance) = skills.getMeanAndVariance
    val beforeTSkillVar = skills.getVariance + skillTransDiagMatrix

    CanonicalGaussian(skillsMean, beforeTSkillVar)
  }

  private def daysDelta(prevTournamentTime: Date, tournamentTime: Date): Long = {

    val timeDelta = (tournamentTime.getTime() - prevTournamentTime.getTime())
    val daysDelta = timeDelta / (1000 * 3600 * 24)
    daysDelta
  }

}
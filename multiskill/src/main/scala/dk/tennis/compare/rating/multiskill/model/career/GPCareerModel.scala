package dk.tennis.compare.rating.multiskill.model.career

import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.factorgraph.GPTennisFactorGraph
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.collection.parallel.ForkJoinTaskSupport

case class GPCareerModel(config: CareerModelConfig) extends CareerModel with Logging {

  def calcTournamentSkills(tournaments: Seq[TournamentResult]): Seq[TournamentSkills] = {

    val parTournaments = tournaments.zipWithIndex.par
    parTournaments.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(1))
   
    val tSkills = parTournaments.map {
      case (t, index) =>

        val lastTournament = tournaments(index).copy(matchResults = Nil)
        val filteredTournaments = tournaments.take(index) :+ lastTournament

        val factorGraph = GPTennisFactorGraph(filteredTournaments, config.pointPerfVarianceOnServe,config.initialSkillsOnServe.mean,config.initialSkillsOnReturn.mean)
        logger.info("Calibrate: " + factorGraph.calibrate(100))

        TournamentSkills(t, getInitialPlayerSkills(t, index, factorGraph))
    }

    tSkills.toList
  }

  private def getInitialPlayerSkills(t: TournamentResult, tIndex: Int, factorGraph: GPTennisFactorGraph): Map[String, PlayerSkills] = {

    val initialPlayerSkills: Map[String, PlayerSkills] = factorGraph.allPlayers.map { p =>
      val skillOnServe = factorGraph.getPlayerGPOnServeMarginal(p).marginal(tIndex + 1).toGaussian
      val skillOnReturn = factorGraph.getPlayerGPOnReturnMarginal(p).marginal(tIndex + 1).toGaussian

      val playerSkills = PlayerSkills(p, t.tournamentTime, PlayerSkill(skillOnServe.m, skillOnServe.v), PlayerSkill(skillOnReturn.m, skillOnReturn.v))
      (p, playerSkills)
    }.toMap

    initialPlayerSkills
  }

}
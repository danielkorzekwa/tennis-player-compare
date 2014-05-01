package dk.tennis.compare.rating.multiskill.model.gpskill

import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.priorskills.PriorSkills

class GenericGPSkillModelTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(1)

  val initialSkillsOnServe = Gaussian(0, 0.8806472793221474)
  val initialSkillsOnReturn = Gaussian(3997.9909513252546 - 4002.542974700307, 0.7527376376092434)

  val pointPerfVarianceOnServe = 102.61914136268837
  val pointPerfVarianceOnReturn = 102.61914136268837

  @Test def test {

    val players: Array[Player] = Player.toPlayers(tournaments)//.take(2)
    logger.info(s"All players in all games: ${players.size}")

    val priorSkills = PriorSkills.priorSkills(players, initialSkillsOnServe, initialSkillsOnReturn)
    println(priorSkills.v)
    val infer = GenericGPSkillsInfer(pointPerfVarianceOnServe, pointPerfVarianceOnReturn, players)
    val skillsGaussianMarginal = infer.skillsMarginal(priorSkills, threshold = 0.6)
    val skillMarginal = skillsGaussianMarginal.m.toArray.zipWithIndex.map {
      case (m, index) => Gaussian(m, skillsGaussianMarginal.v(index, index))
    }

    logger.info("Calculating log likelihood")
    println("Log lik(totalLik,avgLik,pointsTotal): " + LogLik.logLik(skillMarginal, players))

  }

}
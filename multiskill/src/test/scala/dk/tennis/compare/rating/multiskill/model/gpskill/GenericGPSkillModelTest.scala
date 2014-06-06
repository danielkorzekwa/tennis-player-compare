package dk.tennis.compare.rating.multiskill.model.gpskill

import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
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

    val players: Array[Player] = Player.toPlayers(tournaments)
    logger.info(s"All players in all games: ${players.size}")

    val skillsMean = Matrix.zeros(players.size, 1)
    val skillsCov = covarianceMatrix(players)

    val priorSkillsMean = PriorSkills.meanVector(players, initialSkillsOnServe.m, initialSkillsOnReturn.m)
    val priorSkillsCov = PriorSkills.covarianceMatrix(players, players, initialSkillsOnServe.v, initialSkillsOnReturn.v)

    println(priorSkillsCov)
    val priorSkills = MultivariateGaussian(priorSkillsMean, priorSkillsCov)
    val infer = GenericGPSkillsInfer(pointPerfVarianceOnServe, pointPerfVarianceOnReturn, players)
    val skillsFactorGraph = infer.skillsMarginal(priorSkills, threshold = 0.6)

    logger.info("Calculating log likelihood")
    val loglik = infer.loglik(skillsFactorGraph)
    println("Total log lik: " + loglik)

  }

  def covarianceMatrix(players: Array[Player]): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(players(rowIndex), players(colIndex)))
  }

  private def covariance(player1: Player, player2: Player): Double = {

    if (player1.playerName.equals(player1.playerName)) 1d else 0
  }

}
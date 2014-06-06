package dk.tennis.compare.rating.multiskill.model.fastgpskill

import org.junit._
import Assert._
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player

class GenericFastGpSkillTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(5)

  val initialSkillsOnServe = Gaussian(0, 0.8806472793221474)
  val initialSkillsOnReturn = Gaussian(3997.9909513252546 - 4002.542974700307, 0.7527376376092434)

  val pointPerfVarianceOnServe = 102.61914136268837
  val pointPerfVarianceOnReturn = 102.61914136268837

  @Test def test {

    val players: Array[Player] = Player.toPlayers(tournaments)
    logger.info(s"All players in all games: ${players.size}")

    logger.info("Computing skills marginals")
    val infer = GenericFastGpSkill(initialSkillsOnServe, initialSkillsOnReturn,
      pointPerfVarianceOnServe, pointPerfVarianceOnReturn)
    val skillMarginal = infer.skillMarginals(players)

    logger.info("Calculating log likelihood")
   // println("Log lik(totalLik,avgLik,pointsTotal): " + LogLik.logLik(skillMarginal, players))
  }

}
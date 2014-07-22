package dk.tennis.compare.rating.multiskill.model.gpskill

import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.priorskills.PriorSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.naive.NaiveGPSkills
import scala.math._

class GenericGPSkillModelTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(1)

  val initialSkillsOnServe = Gaussian(0, 0.8806472793221474)
  val initialSkillsOnReturn = Gaussian(3997.9909513252546 - 4002.542974700307, 0.7527376376092434)

  val pointPerfVarianceOnServe = 102.61914136268837
  val pointPerfVarianceOnReturn = 102.61914136268837

  @Test def test {

    val players: Array[Player] = toPlayers(tournaments)
    val scores: Array[Score] = toScores(tournaments)
    logger.info(s"All players in all games: ${players.size}")

    val infer = NaiveGPSkills(Array(log(1)), pointPerfVarianceOnServe, pointPerfVarianceOnReturn, players, scores, threshold = 0.6)

    println("skill covariance:" + infer.priorSkillsCov)

    logger.info("Calculating log likelihood")
    val loglik = infer.loglik()
    println("Total log lik: " + loglik)

  }

  def toPlayers(tournaments: Seq[TournamentResult]): Array[Player] = {

    val players = tournaments.flatMap { t =>

      t.matchResults.flatMap { r =>
        val player1OnServe = Player(r.player1, r.player2, onServe = true, r.matchTime)
        val player2OnReturn = Player(r.player2, r.player1, onServe = false, r.matchTime)

        val player2OnServe = Player(r.player2, r.player1, onServe = true, r.matchTime)
        val player1OnReturn = Player(r.player1, r.player2, onServe = false, r.matchTime)

        Array(player1OnServe, player2OnReturn, player2OnServe, player1OnReturn)
      }
    }

    players.toArray
  }

  def toScores(tournaments: Seq[TournamentResult]): Array[Score] = {

    val scores = tournaments.flatMap { t =>

      t.matchResults.flatMap { r =>
        val player1OnServeScore = Score(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon)
        val player2OnServeScore = Score(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon)
        Array(player1OnServeScore, player2OnServeScore)
      }
    }

    scores.toArray
  }

}
package dk.tennis.compare.likelihood

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import LogLikelihoodTest._
import dk.atp.api.domain.MatchComposite
import java.util.Date
import scala.util.Random
import scala.Math._

/**
 * Calculates log likelihood for tennis outcome prediction models.
 *
 */
object LogLikelihoodTest {
  case class PredictionRecord(matchTime: Date, playerA: String, playerB: String, ratingA: Double, ratingB: Double, ratingAWinner: Byte)
}
class LogLikelihoodTest {

  @Test def test {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv");
    val matches = (2009 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatched = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val predictionRecords = generatePredictionRecords(filteredMatched)

    val rand = new Random()
    def calcPlayerAWinnerProb(r: PredictionRecord): Double = 0.5

    val logLikelihood = calcLogLikelihood(predictionRecords, calcPlayerAWinnerProb)
    println("Num of prediction records=%d, logLikelihoodSum=%f, logLikelihoodAvg=%f".format(predictionRecords.size, logLikelihood, logLikelihood / predictionRecords.size))

  }

  private def generatePredictionRecords(matches: Seq[MatchComposite]): Seq[PredictionRecord] = {

    implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0

    val predictionRecords = for {
      m <- matches

      val ratingA = 0
      val ratingB = 0

    } yield PredictionRecord(
      m.tournament.tournamentTime, m.matchFacts.playerAFacts.playerName,
      m.matchFacts.playerBFacts.playerName,
      ratingA, ratingB,
      m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName))

    predictionRecords
  }

  private def calcLogLikelihood(predictionRecords: Seq[PredictionRecord], playerAWinnerProb: (PredictionRecord) => Double): Double = {
    predictionRecords.map { x =>
      val y = x.ratingAWinner
      val h = playerAWinnerProb

      y * log(h(x)) + (1 - y) * log(1 - h(x))
    }.sum
  }
}
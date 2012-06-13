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
import dk.tennis.compare.glicko2._
import dk.tennis.compare.predict.Glicko2TennisPredict;

import dk.tennis.compare.predict.TennisPredict._;
import Glicko2Rating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import scalala.library.Plotting._
import scalala.tensor.dense._
import java.awt.Color
import scala.util.Random
import dk.tennis.compare.predict._

/**
 * Calculates log likelihood for tennis outcome prediction models.
 *
 */
object LogLikelihoodTest {

}
class LogLikelihoodTest {

  @Test def test {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv");

    val matches = (2006 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val rand = new Random()
    val schuffledMatches = filteredMatches.map { m =>
      rand.nextBoolean match {
        case true => {
          val newMatchFacts = m.matchFacts.copy(playerAFacts = m.matchFacts.playerBFacts, playerBFacts = m.matchFacts.playerAFacts)
          m.copy(matchFacts = newMatchFacts)
        }
        case false => m
      }
    }

    val predictionRecords = MarkovTennisPredict.predict(schuffledMatches).filter(r => new DateTime(r.matchTime).getYear() >= 2010)

    val logLikelihood = calcLogLikelihood(predictionRecords)

    /**Print predicted/true probabilities for scatter chart.*/
    // val predictionsGroupedByProb = predictionRecords.flatMap(r => List((r.playerAWinnerProb, r.playerAWinner.toInt), (1 - r.playerAWinnerProb, 1 - r.playerAWinner))).groupBy(r => (r._1 * 100).toInt)
    // val predictionsGroupedByProbSumm = predictionsGroupedByProb.mapValues(r => (r.map(_._2).sum.toDouble / r.size) * 100)
    // predictionsGroupedByProbSumm.keys.toList.sorted.foreach(predictedProb => println("%d,%1.3f".format(predictedProb, predictionsGroupedByProbSumm(predictedProb))))

    /**Print outcome classification records.*/
    //filteredPredictionRecords.flatMap(r => List((r.playerAWinnerProb, r.playerAWinner), (1 - r.playerAWinnerProb, 1 - r.playerAWinner))).foreach(r => println("%1.3f,%d".format(r._1,r._2)))

    println("Num of prediction records=%d, logLikelihoodSum=%f, logLikelihoodAvg=%f".format(predictionRecords.size, logLikelihood, logLikelihood / predictionRecords.size))

  }

  private def calcLogLikelihood(predictionRecords: Seq[PredictionRecord]): Double = {

    val logLikelihood = predictionRecords.map { x =>

      val y = x.playerAWinner
      val h = x.playerAWinnerProb

      y * log(h) + (1 - y) * log(1 - h)
    }.sum

    logLikelihood
  }
}
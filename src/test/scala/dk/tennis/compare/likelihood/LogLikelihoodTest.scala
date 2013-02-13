package dk.tennis.compare.likelihood

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import LogLikelihoodTest._
import dk.atp.api.domain.MatchComposite
import java.util.Date
import scala.util.Random
import dk.tennis.compare.glicko2._
import dk.tennis.compare.predict.Glicko2TennisPredict;

import dk.tennis.compare.predict.TennisPredict._;
import Glicko2Rating._
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import scala.util.Random
import dk.tennis.compare.predict._
import TennisPredict._
import scala.collection._
import java.lang.Math._

/**
 * Calculates log likelihood for tennis outcome prediction models.
 *
 *
 * Dbn 2010-2011, no markets shuffle (Num of prediction records=2550, logLikelihoodSum=-1666.664724, logLikelihoodAvg=-0.653594)
 * Markov 2010-2011, no market shuffle (Num of prediction records=2550, logLikelihoodSum=-1641.429274, logLikelihoodAvg=-0.643698)
 *
 * Dbn 2010-2011 Federer, no market shuffle (Num of prediction records=40, logLikelihoodSum=-16.031987, logLikelihoodAvg=-0.400800)
 * Markov 2010-2011 Federer, no market shuffle (Num of prediction records=40, logLikelihoodSum=-14.085046, logLikelihoodAvg=-0.352126)
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
    val shuffledMatches = filteredMatches.map { m =>
      rand.nextBoolean match {
        case true => {
          val newMatchFacts = m.matchFacts.copy(playerAFacts = m.matchFacts.playerBFacts, playerBFacts = m.matchFacts.playerAFacts)
          m.copy(matchFacts = newMatchFacts)
        }
        case false => m
      }
    }

    //   val matchFilter = (m: MatchComposite) => {
    //    new DateTime(m.tournament.tournamentTime.getTime()).getYear() == 2011 &&
    //      m.matchFacts.containsPlayer("Roger Federer") && m.matchFacts.containsPlayer("Richard Gasquet")
    //   }

    val matchFilter = (m: MatchComposite) => { new DateTime(m.tournament.tournamentTime.getTime()).getYear() >= 2008 }

    val predictionRecordsProgress: mutable.ListBuffer[PredictionRecord] = mutable.ListBuffer()
    val progress = (record: PredictionRecord) => {
      predictionRecordsProgress += record
      val llhProgress = calcLogLikelihood(predictionRecordsProgress) / predictionRecordsProgress.size
      println("Log likelihood= " + llhProgress)
    }
    val predictionRecords = Glicko2TennisPredict.predict(shuffledMatches, matchFilter, progress)

    println("")
    predictionRecords.foreach(println(_))

    val expectedWins = predictionRecords.map(r => r.playerAWinnerProb).sum
    val actualWins = predictionRecords.map(r => r.playerAWinner).sum

    println("\nExpected/actual wins: %f/%d".format(expectedWins, actualWins))

    val logLikelihood = calcLogLikelihood(predictionRecords)

    /**Print predicted/true probabilities for scatter chart.*/
    val predictionsGroupedByProb = predictionRecords.flatMap(r => List((r.playerAWinnerProb, r.playerAWinner.toInt), (1 - r.playerAWinnerProb, 1 - r.playerAWinner))).groupBy(r => (r._1 * 100).toInt)
    val predictionsGroupedByProbSumm = predictionsGroupedByProb.mapValues(r => (r.map(_._2).sum.toDouble / r.size) * 100)
    println("\npredicted_prob,true_prob,sample_size")
    predictionsGroupedByProbSumm.keys.toList.sorted.foreach(predictedProb => println("%d,%.3f,%d".format(predictedProb, predictionsGroupedByProbSumm(predictedProb),
      predictionsGroupedByProb(predictedProb).size)))

    /**Print outcome classification records.*/
    //filteredPredictionRecords.flatMap(r => List((r.playerAWinnerProb, r.playerAWinner), (1 - r.playerAWinnerProb, 1 - r.playerAWinner))).foreach(r => println("%1.3f,%d".format(r._1,r._2)))

    println("\nNum of prediction records=%d, logLikelihoodSum=%f, logLikelihoodAvg=%f".format(predictionRecords.size, logLikelihood, logLikelihood / predictionRecords.size))

  }

}
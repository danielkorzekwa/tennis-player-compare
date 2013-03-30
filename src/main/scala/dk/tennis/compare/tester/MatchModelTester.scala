package dk.tennis.compare.tester

import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import dk.atp.api.domain.MatchComposite
import scala.math._
import MatchModelTester._
import java.util.Date
import scala.collection._
import scala.collection.mutable.StringBuilder
import java.text.SimpleDateFormat

case class MatchModelTester(matchesFile: String, yearFrom: Int, yearTo: Int) {

  private val matches = getMatches()

  def run(matchModel: MatchModel, matchFilter: MatchComposite => Boolean): ModelSummary = {

    val llhStats = LlhStats()
    val predictionRecords: mutable.ListBuffer[PredictionRecord] = mutable.ListBuffer()

    for (m <- matches) {

      if (matchFilter(m)) {
        val playerAFacts = m.matchFacts.playerAFacts
        val playerBFacts = m.matchFacts.playerBFacts
        val matchTime = m.tournament.tournamentTime.getTime

        val playerAWinnerProb = matchModel.matchProb(m)
        val playerAWinner: Byte = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

        playerAWinnerProb.foreach { playerAWinnerProb =>
          llhStats.add(playerAWinner * log(playerAWinnerProb) + (1 - playerAWinner) * log(1 - playerAWinnerProb))

          val predictionRecord = PredictionRecord(m.tournament.tournamentName,
            new Date(matchTime), playerAFacts.playerName,
            playerBFacts.playerName,
            playerAWinnerProb,
            playerAWinner)

          predictionRecords += predictionRecord

        }

      }

      matchModel.addMatchResult(m)
    }

    ModelSummary(predictionRecords.toList, llhStats)

  }

  private def getMatches(): Seq[MatchComposite] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv");

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.matchFacts.playerAFacts.totalServicePointsWon>10 && m.matchFacts.playerBFacts.totalServicePointsWon>10)

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

    shuffledMatches
  }

}

object MatchModelTester {

  case class PredictionRecord(tournament:String,matchTime: Date, playerA: String, playerB: String,
    playerAWinnerProb: Double,
    playerAWinner: Byte)

  case class ModelSummary(predictionRecords: Seq[PredictionRecord], llhStats: LlhStats) {

    def playerAExpectedWins(): Double = predictionRecords.map(r => r.playerAWinnerProb).sum
    def playerActualWins(): Double = predictionRecords.map(r => r.playerAWinner.toInt).sum

    def predictedActualAvgCorrReport(): String = {

      val predictionsGroupedByProb = predictionRecords.flatMap(r => List((r.playerAWinnerProb, r.playerAWinner.toInt), (1 - r.playerAWinnerProb, 1 - r.playerAWinner))).groupBy(r => (r._1 * 100).toInt)
      val predictionsGroupedByProbSumm = predictionsGroupedByProb.mapValues(r => (r.map(_._2).sum.toDouble / r.size) * 100)

      val report = new StringBuilder()
      report.append("\npredicted_prob,true_prob,sample_size\n")
      predictionsGroupedByProbSumm.keys.toList.sorted.foreach(predictedProb =>
        report.append("%d,%.3f,%d\n".format(predictedProb, predictionsGroupedByProbSumm(predictedProb),
          predictionsGroupedByProb(predictedProb).size)))

      report.toString
    }

    def toCSVReport(predictionRecords: List[PredictionRecord]): List[String] = {

      val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

      val csvReport = predictionRecords.map { r =>
        List(df.format(r.matchTime), r.playerA, r.playerB, r.playerAWinner, r.playerAWinnerProb).mkString(",")
      }.toList

      val header = "timestamp,playerA,playerB,playerAWin,playerAWinProb"

      header :: csvReport
    }

  }
  /**
   * Log likelihood stats
   */
  case class LlhStats {
    private var llhTotal = 0d
    private var count = 0

    def add(llh: Double) {
      llhTotal += llh
      count += 1
    }

    override def toString = "LlhStats [llhTotal=%.3f, count=%s,avgLlh=%.3f]".format(llhTotal, count, llhTotal / count)
  }

  implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
}
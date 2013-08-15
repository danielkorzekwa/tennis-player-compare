package dk.tennis.compare.tester

import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import dk.atp.api.domain.MatchComposite
import scala.math._
import GameModelTester._
import java.util.Date
import scala.collection._
import scala.collection.mutable.StringBuilder
import java.text.SimpleDateFormat

case class GameModelTester(results: Seq[GameResult]) {

  private val llhStats = LlhStats()

  def run(gameModel: GameModel, resultFilter: GameResult => Boolean): ModelSummary = {

    val predictionRecords: mutable.ListBuffer[PredictionRecord] = mutable.ListBuffer()

    for (r <- results) {

      if (resultFilter(r)) {

        val playerAWinnerProb = gameModel.gameProb(r)

        playerAWinnerProb.foreach { playerAWinnerProb =>
          val llhValue = if (r.player1Win.get) log(playerAWinnerProb).max(-100) else log1p(-playerAWinnerProb).max(-100)
          if (llhValue.isNaN()) {
            println(llhValue)
          }
          llhStats.add(llhValue)

          val predictionRecord = PredictionRecord(r.eventName.getOrElse("n/a"),
            r.timestamp.get, r.player1,
            r.player2,
            playerAWinnerProb,
            r.player1Win.get)

          predictionRecords += predictionRecord

        }

      }

      gameModel.addGameResult(r)
    }

    ModelSummary(predictionRecords.toList, llhStats)

  }

  def getLlhStats(): LlhStats = llhStats

}

object GameModelTester {

  case class PredictionRecord(tournament: String, matchTime: Date, playerA: String, playerB: String,
    playerAWinnerProb: Double,
    playerAWinner: Byte)

  case class ModelSummary(predictionRecords: Seq[PredictionRecord], llhStats: LlhStats) {

    /**Tuple[expected wins, actual wins]*/
    def expectedVsActualWins(): Tuple2[Double, Int] = {

      val actualVsExpected = predictionRecords.map { r =>
        Random.nextBoolean match {
          case true => (r.playerAWinnerProb, r.playerAWinner.toInt)
          case false => ((1 - r.playerAWinnerProb), if (r.playerAWinner == 1) 0 else 1)
        }
      }

      val actualVsExpectedSummary = actualVsExpected.reduceLeft((sum, a) => (sum._1 + a._1, sum._2 + a._2))
      actualVsExpectedSummary
    }
   
    /**
     * Fraction of players, which were predicted incorrectly as the winner
     */
    def predictionError(): Double = {
      val correctPredictionNum = predictionRecords.map(r =>
        if ((r.playerAWinner == 1) == (r.playerAWinnerProb > 0.5)) 1 else 0).sum
      1 - correctPredictionNum.toDouble / predictionRecords.size
    }

    def playersNum(): Int = predictionRecords.flatMap(r => List(r.playerA, r.playerB)).distinct.size

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

    override def toString = "LlhStats [llhTotal=%.3f, count=%s,avgLlh=%.4f]".format(llhTotal, count, llhTotal / count)
  }

  implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
}
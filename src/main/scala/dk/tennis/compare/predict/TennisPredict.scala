package dk.tennis.compare.predict

import dk.atp.api.domain.MatchComposite
import java.util._
import TennisPredict._
import scala.math._

/**
 * Predicts tennis match winner for a set of matches.
 */
object TennisPredict {

  def calcLogLikelihood(predictionRecords: Seq[PredictionRecord]): Double = {

    val logLikelihood = predictionRecords.map { x =>

      val y = x.playerAWinner
      val h = x.playerAWinnerProb

      //   val logLikelihood = if (y==1) Math.max(-100.0, log(h)) else Math.max(-100.0,log1p(-h))

      //logLikelihood
      y * log(h) + (1 - y) * log(1 - h)
    }.sum

    logLikelihood
  }

  case class PredictionRecord(matchTime: Date, playerA: String, playerB: String,
    playerAWinnerProb: Double,
    playerAWinner: Byte)
}
trait TennisPredict {

  /**
   * Predicts tennis match winner for a set of matches.
   *
   * @param matchFilter Returns prediction records for matches with a match filter criteria equals to true
   * @param progress Listens on produced prediction records.
   */
  def predict(matches: Seq[MatchComposite], matchFilter: (MatchComposite) => Boolean, progress: (PredictionRecord) => Unit): Seq[PredictionRecord]
}
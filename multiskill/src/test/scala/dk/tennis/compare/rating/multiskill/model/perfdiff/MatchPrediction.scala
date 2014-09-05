package dk.tennis.compare.rating.multiskill.model.perfdiff

import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

case class MatchPrediction(p1OnServeScore: Score, p2OnServeScore: Score, p1OnServePerfDiff: Gaussian, p2OnServePerfDiff: Gaussian) {

  val matchTime = p1OnServeScore.player1.timestamp

  def opponentOf(playerName: String): String = {
    if (p1OnServeScore.player1.playerName.equals(playerName)) p1OnServeScore.player1.opponentName
    else if (p2OnServeScore.player1.playerName.equals(playerName)) p1OnServeScore.player2.opponentName
    else throw new IllegalArgumentException("Player not found")
  }

  def hasPlayer(playerName: String): Boolean = p1OnServeScore.hasPlayer(playerName)

  def scoreOnServe(playerName: String) = {
    if (p1OnServeScore.player1.playerName.equals(playerName)) p1OnServeScore
    else if (p2OnServeScore.player1.playerName.equals(playerName)) p2OnServeScore
    else throw new IllegalArgumentException("Player not found")
  }

  def pointProbOnServe(playerName: String) = {
    if (p1OnServeScore.player1.playerName.equals(playerName)) exp(OutcomeLik.loglik(p1OnServePerfDiff, true))
    else if (p2OnServeScore.player1.playerName.equals(playerName)) exp(OutcomeLik.loglik(p2OnServePerfDiff, true))
    else throw new IllegalArgumentException("Player not found")
  }

  def matchProb(playerName: String) = {
    val (p1PointProb, p2PointProb) = if (p1OnServeScore.player1.playerName.equals(playerName)) {
      val p1PointProb = pointProbOnServe(p1OnServeScore.player1.playerName)
      val p2PointProb = pointProbOnServe(p1OnServeScore.player2.playerName)
      (p1PointProb, p2PointProb)
    } else if (p2OnServeScore.player1.playerName.equals(playerName)) {
      val p1PointProb = pointProbOnServe(p1OnServeScore.player2.playerName)
      val p2PointProb = pointProbOnServe(p1OnServeScore.player1.playerName)
      (p1PointProb, p2PointProb)
    } else throw new IllegalArgumentException("Player not found")

    val matchProb = TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    matchProb
  }

  def matchWinner(): Option[String] = {
    val p1TotalPoints = p1OnServeScore.p1PointsWon + p2OnServeScore.p2PointsWon
    val p2TotalPoints = p2OnServeScore.p1PointsWon + p1OnServeScore.p2PointsWon

    val winner = if (p1TotalPoints > p2TotalPoints) Some(p1OnServeScore.player1.playerName)
    else if (p2TotalPoints > p1TotalPoints) Some(p2OnServeScore.player1.playerName)
    else None

    winner

  }

}

object MatchPrediction {

  def toMatchPredictions(scores: Array[Score], perfDiffs: Array[Gaussian]): Seq[MatchPrediction] = {
    val matchPredictions = scores.zip(perfDiffs).grouped(2).map {
      case Array((s1, d1), (s2, d2)) => MatchPrediction(s1, s2, d1, d2)
    }.toList

    matchPredictions
  }
  def totalLogLik(predictions: Seq[MatchPrediction]): Tuple2[Double, Int] = {

    val predictionsWithWinner = predictions.filter(p => p.matchWinner.isDefined)
    val total = predictionsWithWinner.map { p =>
      val winner = p.matchWinner.get

      val matchProb = p.matchProb(winner)
      log(matchProb)
    }.sum

    (total, predictionsWithWinner.size)
  }
}
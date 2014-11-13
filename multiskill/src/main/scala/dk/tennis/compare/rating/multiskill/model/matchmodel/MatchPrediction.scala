package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.Array.canBuildFrom
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenskills.inferMatchProbGivenSkills

case class MatchPrediction(p1OnServeScore: Score, p2OnServeScore: Score, p1OnServePerfDiff: PerfDiff, p2OnServePerfDiff: PerfDiff, matchResult: MatchResult) {

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

  def skillOnServe(playerName: String): Gaussian = {
    if (p1OnServeScore.player1.playerName.equals(playerName)) p1OnServePerfDiff.getP1Skill()
    else if (p2OnServeScore.player1.playerName.equals(playerName)) p2OnServePerfDiff.getP1Skill()
    else throw new IllegalArgumentException("Player not found")
  }

  def skillOnReturn(playerName: String): Gaussian = {
    if (p1OnServeScore.player2.playerName.equals(playerName)) p1OnServePerfDiff.getP2Skill()
    else if (p2OnServeScore.player2.playerName.equals(playerName)) p2OnServePerfDiff.getP2Skill()
    else throw new IllegalArgumentException("Player not found")
  }

  def pointProbOnServe(playerName: String) = {
    if (p1OnServeScore.player1.playerName.equals(playerName)) exp(OutcomeLik.loglik(p1OnServePerfDiff.perfDiff, true))
    else if (p2OnServeScore.player1.playerName.equals(playerName)) exp(OutcomeLik.loglik(p2OnServePerfDiff.perfDiff, true))
    else throw new IllegalArgumentException("Player not found")
  }

  def getPerfDiff(playerNameOnServe: String): PerfDiff = {
    if (p1OnServeScore.player1.playerName.equals(playerNameOnServe)) p1OnServePerfDiff
    else if (p2OnServeScore.player1.playerName.equals(playerNameOnServe)) p2OnServePerfDiff
    else throw new IllegalArgumentException("Player not found")
  }

  def matchProb(playerName: String) = {

    val playerPerfDiff = getPerfDiff(playerName).perfDiff
    val opponentPerfDiff = getPerfDiff(opponentOf(playerName)).perfDiff
    val matchProb = inferMatchProbGivenSkills(playerPerfDiff, opponentPerfDiff, matchResult.numOfSets)

    matchProb
  }

  def matchWinner(): String = {
    if (matchResult.player1Won) matchResult.player1 else matchResult.player2

  }

}

object MatchPrediction {

  def toMatchPredictions(scores: Array[Score], perfDiffs: Array[PerfDiff], matchResults: IndexedSeq[MatchResult]): Seq[MatchPrediction] = {

    val matchPredictions = scores.zip(perfDiffs).grouped(2).zipWithIndex.map {
      case (Array((s1, d1), (s2, d2)), index) => MatchPrediction(s1, s2, d1, d2, matchResults(index))
    }.toList

    matchPredictions
  }
  def totalLogLik(predictions: Seq[MatchPrediction]): Tuple2[Double, Int] = {

    val total = predictions.map { p =>

      val matchProb = p.matchProb(p.matchWinner)
      log(matchProb)
    }.sum

    (total, predictions.size)
  }
}
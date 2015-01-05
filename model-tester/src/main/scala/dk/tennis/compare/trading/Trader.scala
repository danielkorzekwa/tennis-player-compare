package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresultsloo.InferMatchProbGivenMatchResultsLoo
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresultsloo.InferMatchProbGivenMatchResultsLoo
import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStatsDB

case class Trader(betex: Betex, exPricesModel: ExPricesMatchModel, matchResults: Seq[MatchResult]) {

  //val matchModel = InferMatchProbGivenGlicko2Best("./src/test/resources/glicko2_market_probs_2010_2011.csv")
  private val matchModel = InferMatchProbGivenMatchResultsLoo(matchResults.toIndexedSeq)
  //  val matchModel = InferMatchProbGivenPastMatchResults(matchResults.toIndexedSeq)
  // val matchModel = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)

  private val head2HeadStatDB = Head2HeadStatsDB(matchResults)

  def processMatchResult(result: MatchResult) {

    val exPrices = exPricesModel.gamePrices(result)
    if (exPrices.isEmpty) return

    val overround = 1d / exPrices.get.p1Price + 1d / exPrices.get.p2Price
    if (!(overround > 1 && overround < 1.01)) return

    val matchPrediction = matchModel.predict(result)
  //  if (matchPrediction.p1OnServePerfDiff.getP1Skill.v < 0.5 || matchPrediction.p2OnServePerfDiff.getP1Skill.v < 0.5) return

    //player1 bet
    val p1Price = exPrices.get.p1Price
    val p1TrueProb = matchPrediction.matchProb(result.player1)
    val p1win = result.player1Won
    val p1Outcome = Outcome(p1Price, p1TrueProb, p1win)
    val p1HeadToHeadStat = head2HeadStatDB.getH2HStat(result.player1, result.player2, result.matchTime)
    placeBet(p1Outcome, p1HeadToHeadStat)

    //player2 bet
    val p2Price = exPrices.get.p2Price
    val p2TrueProb = matchPrediction.matchProb(result.player2)
    val p2win = !result.player1Won
    val p2Outcome = Outcome(p2Price, p2TrueProb, p2win)
    val p2HeadToHeadStat = head2HeadStatDB.getH2HStat(result.player2, result.player1, result.matchTime)
    placeBet(p2Outcome, p1HeadToHeadStat)

    //stats print
    println("betsNum=%d, profit=%.2f, expWins/actWins%.2f/%.2f, loglik/loglikEx: %.2f / %.2f, result=%s, p1Skill=%s,p2Skill=%s, exPrices=%.2f/%.2f".format(
      betex.getBetsNum, betex.getProfit,
      betex.expectedExWins, betex.actualWins,
      betex.loglik.getAvg, betex.loglikEx.getAvg,
      result, matchPrediction.p1OnServePerfDiff.getP1Skill, matchPrediction.p2OnServePerfDiff.getP1Skill,
      exPrices.get.p1Price, exPrices.get.p2Price))

  }

  def placeBet(outcome: Outcome, h2hStat: Head2HeadStat) {
    val price = outcome.price
    val trueProb = outcome.trueProb

    if (price < 3 & price > 1.1 && price * outcome.trueProb > 1.1) betex.placeBet(outcome)

  }

}
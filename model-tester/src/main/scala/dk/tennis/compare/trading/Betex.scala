package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.OnlineAvg
import scala.math._

case class Betex(commission: Double) {

  private var totalProfit = 0d
  private var betsNum = 0

  var expectedExWinsBack = 0d
  var actualWinsBack = 0d

  var expectedExWinsLay = 0d
  var actualWinsLay = 0d

  val loglik = OnlineAvg()
  val loglikEx = OnlineAvg()

  def placeBet(outcome: Outcome, backBet: Boolean) {
    val backPrice = outcome.backPrice
    val layPrice = outcome.layPrice
    val outcomeBit = if (outcome.win) 1 else 0
    val stake = 1

    val betProfit = if (backBet) {

      expectedExWinsBack += 1d / backPrice
      actualWinsBack += outcomeBit

      val betProfit = stake * backPrice * outcomeBit - stake
      if (betProfit > 0) betProfit * (1 - commission) else betProfit

    } else {

      expectedExWinsLay += 1d / layPrice
      actualWinsLay += outcomeBit
      val betProfit = -stake * layPrice * outcomeBit - (-stake)
      if (betProfit > 0) betProfit * (1 - commission) else betProfit
    }

    totalProfit += betProfit
    betsNum += 1

    //update log likelihood
    val avgExPriceProb = (1d / backPrice + 1d / layPrice) / 2
    val winnerProbEx = if (outcomeBit == 1) avgExPriceProb else 1 - avgExPriceProb
    val winnerProb = if (outcomeBit == 1) outcome.trueProb else 1 - outcome.trueProb
    loglik.add(log(winnerProb))
    loglikEx.add(log(winnerProbEx))

  }

  def getProfit(): Double = totalProfit
  def getBetsNum(): Int = betsNum
}
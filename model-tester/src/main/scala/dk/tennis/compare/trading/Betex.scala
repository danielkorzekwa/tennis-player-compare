package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.OnlineAvg
import scala.math._

case class Betex(commission: Double) {

  private var totalProfit = 0d
  private var betsNum = 0

  var expectedExWins = 0d
  var actualWins = 0d

  val loglik = OnlineAvg()
  val loglikEx = OnlineAvg()

  def placeBet(outcome: Outcome) {
    val price = outcome.price
    val outcomeBit = if (outcome.win) 1 else 0
    val stake = 1

    val betProfit =  {

      expectedExWins += 1d / price
      actualWins += outcomeBit

      val betProfit = stake * price * outcomeBit - stake
      if (betProfit > 0) betProfit * (1 - commission) else betProfit

    } 

    totalProfit += betProfit
    betsNum += 1

    //update log likelihood
    val avgExPriceProb = 1d/price
    val winnerProbEx = if (outcomeBit == 1) avgExPriceProb else 1 - avgExPriceProb
    val winnerProb = if (outcomeBit == 1) outcome.trueProb else 1 - outcome.trueProb
    loglik.add(log(winnerProb))
    loglikEx.add(log(winnerProbEx))

  }

  def getProfit(): Double = totalProfit
  def getBetsNum(): Int = betsNum
}
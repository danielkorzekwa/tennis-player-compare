package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat

case class Trader {

  private var totalProfit = 0d
  private var betsNum = 0

  def placeBet(outcome: Outcome, h2hStat: Head2HeadStat) {
    val price = outcome.price
    val trueProb = outcome.trueProb
    val outcomeBit = if (outcome.win) 1 else 0
    val stake = 1

    val betProfit =
      if (price * outcome.trueProb > 1.5) stake * price * outcomeBit - stake //back bet 
      else if (price * outcome.trueProb < 0.5) -stake * price * outcomeBit - (-stake) //lay bet
      else 0
    //   if (1d / outcome.trueProb < price) stake * price * outcomeBit - stake //back bet 
    //   else -stake * price * outcomeBit - (-stake) //lay bet

    if (betProfit != 0) {
      totalProfit += betProfit

      betsNum += 1
    }
  }

  def getProfit(): Double = totalProfit
  def getBetsNum(): Int = betsNum
}
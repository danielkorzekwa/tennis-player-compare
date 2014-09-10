package dk.tennis.compare.trading

case class Trader {

  private var totalProfit = 0d
  private var betsNum = 0

  def placeBet(outcome: Outcome) {
    val price = outcome.price.get
    val trueProb = outcome.trueProb
    val outcomeBit = if (outcome.win) 1 else 0
    val stake = 1

    val betProfit =
      if (1d / outcome.trueProb < price) stake * price * outcomeBit - stake //back bet 
      else -stake * price * outcomeBit - (-stake) //lay bet

    totalProfit += betProfit
    betsNum += 1
  }

  def getProfit(): Double = totalProfit
  def getBetsNum(): Int = betsNum
}
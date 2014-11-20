package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat

case class Trader {

  private var totalProfit = 0d
  private var betsNum = 0

  var expectedExWinsBack = 0d
  var actualWinsBack = 0d

  var expectedExWinsLay = 0d
  var actualWinsLay = 0d

  def placeBet(outcome: Outcome, h2hStat: Head2HeadStat) {
    val price = outcome.price
    val trueProb = outcome.trueProb
    val outcomeBit = if (outcome.win) 1 else 0
    val stake = 1

    val betProfit =
      if (price * outcome.trueProb > 1.05) {
     

        expectedExWinsBack += 1 / price
        actualWinsBack += outcomeBit
        
           stake * price * outcomeBit - stake //back bet
        
      } else if (price * outcome.trueProb < 0.95) {
        

        expectedExWinsLay += 1 / price
        actualWinsLay += outcomeBit
        
        -stake * price * outcomeBit - (-stake) //lay bet
      } else 0

    if (betProfit != 0) {
      totalProfit += betProfit

      betsNum += 1
    }
  }

  def getProfit(): Double = totalProfit
  def getBetsNum(): Int = betsNum
}
package dk.tennis.compare.trading

import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat

case class Trader(betex: Betex) {

  def placeBet(outcome: Outcome, h2hStat: Head2HeadStat) {
    val backPrice = outcome.backPrice
    val layPrice = outcome.layPrice
    val trueProb = outcome.trueProb

    if (backPrice * outcome.trueProb > 1.05) betex.placeBet(outcome, backBet = true)

    else if (layPrice * outcome.trueProb < 0.95) betex.placeBet(outcome, backBet = false)

  }

}
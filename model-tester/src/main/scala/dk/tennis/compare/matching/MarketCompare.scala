package dk.tennis.compare.matching

import dk.tennis.compare.domain.BfMarket
import dk.atp.api.domain.TennisMatch

trait MarketCompare {

  /**
   * Returns the probability of matching atp and betfair markets.
   */
  def compare(atpMarket: TennisMatch, betfairMarket:BfMarket):Double
}
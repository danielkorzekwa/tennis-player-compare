package dk.tennis.compare.matching

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.domain.Market


trait MarketCompare {

  /**
   * Returns the probability of matching atp and betfair markets.
   */
  def compare(atpMarket: MatchComposite, betfairMarket:Market):Double
}
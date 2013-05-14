package dk.tennis.compare.game.tennis.matching

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.game.tennis.domain.BfMarket

trait MarketCompare {

  /**
   * Returns the probability of matching atp and betfair markets.
   */
  def compare(atpMarket: MatchComposite, betfairMarket:BfMarket):Double
}
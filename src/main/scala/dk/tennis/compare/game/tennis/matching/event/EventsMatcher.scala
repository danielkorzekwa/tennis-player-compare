package dk.tennis.compare.game.tennis.matching.event

/**
 * Matches events (tournaments) between ATP website and Betfair Exchange.
 *
 * @author Daniel Korzekwa
 */
trait EventsMatcher {

  /**
   * Matches events (tournaments) between ATP website and Betfair Exchange.
   *
   * @return Matching probability. Between 0 and 1
   */
  def matchEvents(atpEventName: String, betfairEventId: String, eventYear:Int): Double
}
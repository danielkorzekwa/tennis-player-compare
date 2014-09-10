package dk.tennis.compare.matching.event

import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import dk.tennis.compare.domain.BfMarket
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

/**
 * Default implementation for the EventsMatcher.
 *
 * @author Daniel Korzekwa
 *
 */
case class GenericEventsMatcher(matchResults: Seq[MatchResult], betfairMarkets: Seq[BfMarket]) extends EventsMatcher {

  /**Map[atpEventKey+bfEventKey,matchingProb]*/
  private val matchingProbs: Map[String, Double] = calcMatchingProbs()

  def matchEvents(atpEventName: String, betfairEventName: String, eventYear: Int): Double = {
    val prob = try {
      matchingProbs(getEventKey(atpEventName, eventYear).concat(getEventKey(betfairEventName, eventYear)))
    } catch {
      case e: NoSuchElementException => 0
    }
    prob
  }

  /**
   * Returns Map[atpEventName+bfEventName,matchingProb]
   */
  private def calcMatchingProbs(): Map[String, Double] = {

    /**Map[bfEventKey,event markets]*/
    val atpMarketsByEvent = matchResults.groupBy(m => getEventKey(m.tournamentName, new DateTime(m.tournamentTime.getTime).getYear))
    val bfMarketsByEvent = betfairMarkets.groupBy(m => getEventKey(m.eventName, new DateTime(m.scheduledOff).getYear))

    val matchingProbsList = for {
      atpTournament <- atpMarketsByEvent.keys.toList
      bfEvent <- bfMarketsByEvent.keys.toList

      val eventPairKey = atpTournament.concat(bfEvent)
      val matchingProb = calcMatchingProb(atpMarketsByEvent(atpTournament), bfMarketsByEvent(bfEvent))

    } yield (eventPairKey, matchingProb)

    Map(matchingProbsList.toSeq: _*)
  }

  /**Map[atpEventKey+bfEventKey,matchingProb]*/
  def getMatchingProbs(): Map[String, Double] = matchingProbs

  /**
   * Returns the probability of atp and betfair markets coming from the same event.
   */
  private def calcMatchingProb(atpMarkets: Seq[MatchResult], betfairMarkets: Seq[BfMarket]): Double = {

    val atpPlayerPairs = atpMarkets.map(m => List(m.player1.toLowerCase, m.player2.toLowerCase).sorted.mkString(":"))
    val bfPlayerPairs = betfairMarkets.map(m => m.runnerMap.values.map(r => r.name.toLowerCase).toList.sorted.mkString(":"))
    val pairIntersection = atpPlayerPairs.intersect(bfPlayerPairs)
    val matchedPairsRatio = pairIntersection.size.toDouble / atpPlayerPairs.size
    matchedPairsRatio
  }
  private def getEventKey(eventName: String, year: Int): String = eventName + year
}

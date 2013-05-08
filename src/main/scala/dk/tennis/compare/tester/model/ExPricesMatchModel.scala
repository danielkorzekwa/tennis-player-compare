package dk.tennis.compare.tester.model

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.domain.Market
import dk.tennis.compare.matching.GenericMarketCompare
import dk.tennis.compare.matching.GenericMarketCompare
import dk.tennis.compare.matching.event.GenericEventsMatcher
import dk.tennis.compare.tester.MatchModel
import org.joda.time.DateTime
import dk.tennis.compare.matching.playerspair.GenericPlayersPairMatcher
import org.slf4j.LoggerFactory

case class ExPricesMatchModel(atpMarkets: Seq[MatchComposite], bfMarkets: Seq[Market]) extends MatchModel {

  private val log = LoggerFactory.getLogger(getClass)
  val eventsMatcher = GenericEventsMatcher(atpMarkets, bfMarkets)

  def matchProb(m: MatchComposite): Option[Double] = {

    val playersPairAtp = Tuple2(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName)
    val atpEventName = m.tournament.tournamentName
    val atpEventYear = new DateTime(m.tournament.tournamentTime).getYear

    val matchedBfMarkets = bfMarkets.filter { bfMarket =>

      val bfPlayers = bfMarket.runnerMap.values.toList
      val playersPairBf = Tuple2(bfPlayers(0).name, bfPlayers(1).name)
      val bfEventYear = new DateTime(bfMarket.scheduledOff).getYear()

      val matched = atpEventYear == bfEventYear &&
        eventsMatcher.matchEvents(atpEventName, bfMarket.eventName, atpEventYear) > 0.5 &&
        GenericPlayersPairMatcher.matchPlayersPair(playersPairAtp, playersPairBf)

      matched
    }

    matchedBfMarkets match {

      case List(bfMarket) => {
        val price = bfMarket.runnerMap.values.find(r => r.name.toLowerCase() == m.matchFacts.playerAFacts.playerName.toLowerCase()).get.price
        Some(1d / price)
      }
      case Nil => None
      case _ => {
        log.warn("ATP market is matched with multiple betfair markets. Market: %s is matched with markets: %s".format(m, matchedBfMarkets))
        None
      }
    }

  }
  def addMatchResult(matchComposite: MatchComposite) {}
}
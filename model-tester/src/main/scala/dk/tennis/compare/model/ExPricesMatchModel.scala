package dk.tennis.compare.model

import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import dk.tennis.compare.domain.BfMarket
import dk.tennis.compare.matching.event.GenericEventsMatcher
import dk.tennis.compare.matching.playerspair.GenericPlayersPairMatcher
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

case class ExPricesMatchModel(atpTournaments: Seq[TournamentResult], bfMarkets: Seq[BfMarket]) {

  private val log = LoggerFactory.getLogger(getClass)
  val eventsMatcher = GenericEventsMatcher(atpTournaments, bfMarkets)

  def gameProb(tournament: TournamentResult, matchResult: MatchResult): Option[Double] = {

    val playersPairAtp = Tuple2(matchResult.player1, matchResult.player2)
    val atpEventName =tournament.tournamentName
    val atpEventYear = new DateTime(tournament.tournamentTime.getTime).getYear

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
        val price = bfMarket.runnerMap.values.find(runner => runner.name.toLowerCase() == matchResult.player1.toLowerCase()).get.price
        Some(1d / price)
      }
      case Nil => None
      case _ => {
        log.warn("ATP market is matched with multiple betfair markets. Market: %s is matched with markets: %s".format(matchResult, matchedBfMarkets))
        None
      }
    }

  }
 
}
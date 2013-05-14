package dk.tennis.compare.game.tennis.model

import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.matching.event.GenericEventsMatcher
import dk.tennis.compare.game.tennis.matching.playerspair.GenericPlayersPairMatcher
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennis.compare.game.tennis.domain.BfMarket

case class ExPricesMatchModel(atpMarkets: Seq[TennisResult], bfMarkets: Seq[BfMarket]) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)
  val eventsMatcher = GenericEventsMatcher(atpMarkets, bfMarkets)

   def gameProb(r: GameResult):Option[Double] = {

    val playersPairAtp = Tuple2(r.player1,r.player2)
    val atpEventName = r.eventName
    val atpEventYear = new DateTime(r.timestamp.get).getYear

    val matchedBfMarkets = bfMarkets.filter { bfMarket =>

      val bfPlayers = bfMarket.runnerMap.values.toList
      val playersPairBf = Tuple2(bfPlayers(0).name, bfPlayers(1).name)
      val bfEventYear = new DateTime(bfMarket.scheduledOff).getYear()

      val matched = atpEventYear == bfEventYear &&
        eventsMatcher.matchEvents(atpEventName.get, bfMarket.eventName, atpEventYear) > 0.5 &&
        GenericPlayersPairMatcher.matchPlayersPair(playersPairAtp, playersPairBf)

      matched
    }

    matchedBfMarkets match {

      case List(bfMarket) => {
        val price = bfMarket.runnerMap.values.find(runner => runner.name.toLowerCase() == r.player1.toLowerCase()).get.price
        Some(1d / price)
      }
      case Nil => None
      case _ => {
        log.warn("ATP market is matched with multiple betfair markets. Market: %s is matched with markets: %s".format(r, matchedBfMarkets))
        None
      }
    }

  }
  def addGameResult(r:GameResult) {}
}
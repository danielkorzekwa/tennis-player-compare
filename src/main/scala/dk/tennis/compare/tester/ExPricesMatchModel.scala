package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.domain.Market
import scala.io.Source
import dk.tennis.compare.matching.GenericMarketCompare

case class ExPricesMatchModel(betfairMatchesFile: String) extends MatchModel {

  val marketDataSource = Source.fromFile(betfairMatchesFile)
  val bfMarkets = Market.fromCSV(marketDataSource.getLines().drop(1).toList)

  def matchProb(m: MatchComposite): Option[Double] = {

    val matchedBfMarkets = bfMarkets.filter(bfMarket => GenericMarketCompare.compare(m, bfMarket) > 0.4)
    require(matchedBfMarkets.size <= 1, "ATP market is matched with multiple betfair markets. Market: %s is matched with markets: %s".format(m,matchedBfMarkets))

    matchedBfMarkets match {
      case Nil => None
      case List(bfMarket) => {
        val price = bfMarket.runnerMap.values.find(r => r.name == m.matchFacts.playerAFacts.playerName).get.price
        Some(1d / price)
      }
    }

  }
  def addMatchResult(matchComposite: MatchComposite) {}
}
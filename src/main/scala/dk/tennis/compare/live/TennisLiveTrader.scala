package dk.tennis.compare.live

import scala.collection.JavaConversions._
import org.slf4j._
import dk.bot.betfairservice.BetFairService
import dk.bot.betfairservice.model._
import java.util.Date
import java.util.Arrays
import java.util.HashSet
import dk.bot.betfairservice.model.BFMUBet
import dk.bot.betfairservice.model.BFBetStatus
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.io.Source
import dk.atp.api.facts._
import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.atp.api.domain._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare._
import java.util.Date
import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.atp.api._
import TennisLiveTrader._

object TennisLiveTrader {

  case class MarketDetails(eventName: String, surface: SurfaceEnum, matchType: MatchTypeEnum)
  case class CompositeMarket(marketData: BFMarketData, marketRunners: BFMarketDetails, marketDetails: Option[MarketDetails], probability: Option[Double])

}

/**Tests TennisMatchCompare live by placing real bets on Betfair.*/
class TennisLiveTrader {

  private val MAIN_LOOP_INTERVAL_IN_SEC = 60
  private val marketDetailsFile = "./target/market_details.csv"
  private val atpMatchesFile = "./target/matches.csv"

  private val log = LoggerFactory.getLogger(getClass)

  private lazy val betfairService = createBetfairService()

  def run() {
    while (true) {
      val startFrom = new Date(System.currentTimeMillis + 1000 * 60) //in 1 minute
      val startTo = new Date(System.currentTimeMillis + 1000 * 3600 * 24 * 3) //in 3 days
      val markets = loadBetfairTennisMarkets(startFrom, startTo)

      val marketsWithDetails = matchMarketsWithDetails(markets)
      val marketsWithDetailsAndProbs = matchMarketsWithProbability(marketsWithDetails)

      val bets = loadCurrentBets()
      log.info(markets.size + ":" + marketsWithDetails.size + ":" + marketsWithDetailsAndProbs.size)
      Thread.sleep(MAIN_LOOP_INTERVAL_IN_SEC * 1000)
    }
  }

  def createBetfairService(): BetFairService = {
    /**Create betfair service and login to betfair account.*/
    val betfairServiceFactoryBean = new dk.bot.betfairservice.DefaultBetFairServiceFactoryBean();
    betfairServiceFactoryBean.setUser(System.getProperty("bfUser"))
    betfairServiceFactoryBean.setPassword(System.getProperty("bfPassword"))
    betfairServiceFactoryBean.setProductId(System.getProperty("bfProductId").toInt)
    val loginResponse = betfairServiceFactoryBean.login
    val betfairService: BetFairService = (betfairServiceFactoryBean.getObject.asInstanceOf[BetFairService])
    betfairService
  }

  def loadBetfairTennisMarkets(startFrom: Date, startTo: Date): List[CompositeMarket] = {
    log.info("Loading betfair markets...")
    val eventTypeIds: java.util.Set[Integer] = Set(new Integer(2))
    val markets = betfairService.getMarkets(startFrom, startTo, eventTypeIds)
    val compositeMarkets = markets.map { m =>
      val marketDetails = betfairService.getMarketDetails(m.getMarketId())
      CompositeMarket(m, marketDetails, None, None)
    }
    log.info("Loading betfair markets = " + compositeMarkets.size)
    compositeMarkets.toList
  }

  def loadCurrentBets(): List[BFMUBet] = {
    val bets = betfairService.getMUBets(BFBetStatus.MU)
    bets.toList
  }

  def loadSurfaceAndMatchType(): List[MarketDetails] = {
    val marketDetailsSource = Source.fromFile(marketDetailsFile)

    val marketDetails = marketDetailsSource.getLines().drop(1).map { line =>
      val lineArray = line.split(",")
      val event = lineArray(0)
      val surface = lineArray(1) match {
        case "HARD" => HARD
        case "GRASS" => GRASS
        case "CLAY" => CLAY
      }
      val matchType = lineArray(2) match {
        case "2" => THREE_SET_MATCH
        case "3" => FIVE_SET_MATCH
      }
      MarketDetails(event, surface, matchType)
    }
    marketDetails.toList
  }

  def matchMarketsWithDetails(markets: List[CompositeMarket]): List[CompositeMarket] = {
    val marketDetails = loadSurfaceAndMatchType()

    val marketsWithDetails = for {
      market <- markets
      val matchedMarketDetails = marketDetails.find(md => market.marketData.getMenuPath().contains(md.eventName))
      if (matchedMarketDetails.isDefined)
    } yield market.copy(marketDetails = Some(matchedMarketDetails.get))

    marketsWithDetails
  }

  def matchMarketsWithProbability(markets: List[CompositeMarket]): List[CompositeMarket] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(atpMatchesFile)
    val matchCompare = new GlickoSoftTennisMatchCompare(atpMatchesLoader, 60, 1500, 100, 9.607, 7)
    matchCompare

    val marketsWithProb = markets.map { m =>
      val playerA = m.marketRunners.getRunners().get(0).getSelectionName()
      val playerB = m.marketRunners.getRunners().get(1).getSelectionName()

      val prob = try {
        val prob = matchCompare.matchProb(playerA, playerB, m.marketDetails.get.surface, m.marketDetails.get.matchType, m.marketData.getEventDate())
        Some(prob)
      } catch {
        case e: java.util.NoSuchElementException => None
      }
      m.copy(probability = prob)

    }
    marketsWithProb.filter(m => m.probability.isDefined)
  }
}
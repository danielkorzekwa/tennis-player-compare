package dk.tennis.compare.old.live

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
import dk.tennis.compare.rating.glicko._
import dk.tennis.compare.old.bulkcompare.GlickoSoftTennisMatchCompare

object TennisLiveTrader {

  case class MarketDetails(eventName: String, surface: SurfaceEnum, matchType: MatchTypeEnum)

  /**@param probability Map[runnerName,probability]*/
  case class CompositeMarket(marketData: BFMarketData, marketRunners: BFMarketDetails, marketDetails: Option[MarketDetails], marketPrices: Option[BFMarketRunners], tradedVolume: Option[BFMarketTradedVolume], probability: Option[Map[String, Double]])
  case class Bet(marketId: Int, selectionId: Int, price: Double, truePrice: Double, tradedVolume: Double, priceToLay: Double)
}

/**Tests TennisMatchCompare live by placing real bets on Betfair.*/
class TennisLiveTrader {

  private val MAIN_LOOP_INTERVAL_IN_SEC = 60
  private val MARKET_TIME_TO_IN_HOURS = 24
  private val marketDetailsFile = "./market_details.csv"
  private val atpMatchesFile = "./matches.csv"

  private val log = LoggerFactory.getLogger(getClass)

  /**key - timestamp + surface.*/
  private val placedBets: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  private lazy val betfairService = createBetfairService()

  def run() {
    while (true) {
      try {

        val startFrom = new Date(System.currentTimeMillis + 1000 * 60) //in 1 minute
        val startTo = new Date(System.currentTimeMillis + 1000 * 3600 * MARKET_TIME_TO_IN_HOURS) //in 3 days
        val markets = loadBetfairTennisMarkets(startFrom, startTo)

        val marketsWithDetails = matchMarketsWithDetails(markets)
        val marketsWithDetailsAndProbs = matchMarketsWithProbability(marketsWithDetails)

        val marketsWithTradedVolume = matchWithTradedVolumeAndPrices(marketsWithDetailsAndProbs)

        val allBets = mapToBets(marketsWithTradedVolume)
        val filteredBets = allBets.filter(b => b.tradedVolume > 1000 && b.price < 20 && b.price / b.truePrice > 1.02)
        log.info("Generating bets matching betting criteria  = " + filteredBets.size)

        val unmatchedBets = filteredBets.filter(b => b.price >= b.priceToLay)
        log.info("Generating unmatched bets only  = " + unmatchedBets.size)

        val currentBets = loadCurrentBets()
        val betsNotPlacedYet = unmatchedBets
          .filterNot(b => currentBets.exists(cb => cb.getMarketId() == b.marketId && cb.getSelectionId() == b.selectionId && cb.getPrice() == b.price))
          .filterNot(b => placedBets.contains(b.marketId + ":" + b.selectionId + ":" + b.price))
        log.info("Generating bets not placed yet  = " + betsNotPlacedYet.size)

        log.info("Placing bets = " + betsNotPlacedYet.size)
        betsNotPlacedYet.foreach { b =>
          betfairService.placeBet(b.marketId, b.selectionId, BFBetType.B, b.price, 2, false)
          placedBets += b.marketId + ":" + b.selectionId + ":" + b.price
        }

        log.info("Placed bets = " + betsNotPlacedYet.size)
        println(betsNotPlacedYet.mkString("\n"))
        Thread.sleep(MAIN_LOOP_INTERVAL_IN_SEC * 1000)
      } catch {
        case e: Exception => log.error("Error", e)
      }
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
    val markets = betfairService.getMarkets(startFrom, startTo, eventTypeIds).filter(m => m.getMarketName().equals("Match Odds"))
    val compositeMarkets = markets.map { m =>
      val marketDetails = betfairService.getMarketDetails(m.getMarketId())
      CompositeMarket(m, marketDetails, None, None, None, None)
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
    log.info("Matching probabilities...")

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(atpMatchesFile)
    val glickoLoader = CachedGlickoRatingsLoader(atpMatchesLoader, 60, 1500, 100, 9.607, 7)
    val matchCompare = new GlickoSoftTennisMatchCompare(glickoLoader)
    matchCompare

    val marketsWithProb = markets.map { m =>
      val playerA = m.marketRunners.getRunners().get(0).getSelectionName()
      val playerB = m.marketRunners.getRunners().get(1).getSelectionName()

      val prob = try {
        val prob = matchCompare.matchProb(playerA, playerB, m.marketDetails.get.surface, m.marketDetails.get.matchType, m.marketData.getEventDate())
        Some(Map(playerA -> prob, playerB -> (1 - prob)))
      } catch {
        case e: java.util.NoSuchElementException => None
      }
      m.copy(probability = prob)

    }
    val matchedMarkets = marketsWithProb.filter(m => m.probability.isDefined)
    log.info("Matching probabilities = " + marketsWithProb.size + "/" + matchedMarkets.size)
    matchedMarkets
  }

  def matchWithTradedVolumeAndPrices(markets: List[CompositeMarket]): List[CompositeMarket] = {
    log.info("Matching with traded volume and prices...")
    val matchedMarkets = markets.map { m =>
      val marketTradedVolume = betfairService.getMarketTradedVolume(m.marketData.getMarketId())
      val marketPricesObject = betfairService.getMarketRunners(m.marketData.getMarketId())
      m.copy(marketPrices = Some(marketPricesObject), tradedVolume = Some(marketTradedVolume))
    }
    log.info("Matching with traded volume and prices... - done")
    matchedMarkets
    val filteredMarkets = matchedMarkets.filter(m => m.marketPrices.get.getInPlayDelay() == 0)
    log.info("Filtering not inplay markets = " + matchedMarkets.size)
    filteredMarkets
  }

  def mapToBets(markets: List[CompositeMarket]): List[Bet] = {

    val bets: List[Bet] = markets.flatMap { m =>
      val marketBets = m.tradedVolume.get.getRunnerTradedVolume().flatMap { runnerTv =>
        val runnerBets = runnerTv.getPriceTradedVolume().map { priceTv =>
          val truePrice = getTruePrice(m, runnerTv.getSelectionId())
          val currentPrice = m.marketPrices.get.getMarketRunner(runnerTv.getSelectionId()).getPriceToLay()
          val bet = Bet(m.marketData.getMarketId(), runnerTv.getSelectionId(), priceTv.getPrice(), truePrice, priceTv.getTradedVolume(), currentPrice)
          bet
        }
        runnerBets
      }
      marketBets
    }
    log.info("Generating all bets = " + bets.size)
    bets

  }

  def getTruePrice(market: CompositeMarket, selectionId: Int): Double = {
    val runner = market.marketRunners.getRunners().find(r => r.getSelectionId() == selectionId).get
    val probability = market.probability.get(runner.getSelectionName())
    1 / probability
  }
}
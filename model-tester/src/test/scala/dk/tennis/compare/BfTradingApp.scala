package dk.tennis.compare

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import scala.io.Source
import dk.tennis.compare.domain.BfMarket
import dk.tennis.compare.matching.event.GenericEventsMatcher
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiff
import dk.tennis.compare.trading.Outcome
import dk.tennis.compare.trading.tradingSim
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.trading.Trader
import dk.tennis.compare.rating.multiskill.model.matchmodel.LooMatchModel
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.model.matchmodel.PastDataMatchModel

object BfTradingApp extends App with Logging {

  logger.info("Starting BfTradingApp")

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)

  val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
  val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)

  val exPricesModel = ExPricesMatchModel(matchResults, bfMarkets)

  //val matchModel = LooMatchModel(matchResults.toIndexedSeq)
  val matchModel = PastDataMatchModel(matchResults.toIndexedSeq)

  run()

  def run() {

    val trader = Trader()

    matchResults.foreach { result =>
      val exProb = exPricesModel.gameProb(result)

      if (exProb.isDefined) {
        val matchPrediction = matchModel.predict(result)

        val price = if (exProb.isDefined) Some(1d / exProb.get) else None
        val trueProb = matchPrediction.matchProb(result.player1)
        val win = result.player1Won

        val outcome = Outcome(price, trueProb, win)
        trader.placeBet(outcome)

        println(trader.getBetsNum + "," + trader.getProfit)
      }
    }

  }

}
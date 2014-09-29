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
import dk.tennis.compare.trading.Outcome
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.trading.Trader
import dk.tennis.compare.model.ExPricesMatchModel
import scala.util.Random
import dk.tennis.compare.rating.multiskill.analysis.OnlineAvg
import scala.math._
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.model.matchmodel.LooMatchModel
import scala.collection.immutable.HashSet

object BfTradingApp extends App with Logging {

  logger.info("Starting BfTradingApp")

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val playersSet = HashSet("Roger Federer", "Andy Roddick")
  val matchResults = shuffle(MatchesLoader.loadMatches(matchesFile, 2008, 2011, playersSet))
  logger.info("Matches=" + matchResults.size)
  val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
  val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)

  val exPricesModel = ExPricesMatchModel(matchResults, bfMarkets)

  val matchModel = LooMatchModel(matchResults.toIndexedSeq)
  // val matchModel = PastDataMatchModel(matchResults.toIndexedSeq)

  run()

  def run() {

    //trading simulation

    val trader = Trader()
    val loglik = OnlineAvg()
    matchResults.foreach { result =>
      val exPrices = exPricesModel.gamePrices(result)

      if (exPrices.isDefined) {
        val matchPrediction = matchModel.predict(result)

        val p1Price = exPrices.get.p1Price
        val p1TrueProb = matchPrediction.matchProb(result.player1)
        val win = result.player1Won
        val outcome = Outcome(p1Price, p1TrueProb, win)
        trader.placeBet(outcome)
        //println(trader.getBetsNum + "," + trader.getProfit)

        val winnerProb = matchPrediction.matchProb(matchPrediction.matchWinner)
        loglik.add(log(winnerProb))
        println("loglik: " + loglik.getAvg)
      }
    }

    matchResults.foreach { result =>
      val exPrices = exPricesModel.gamePrices(result)

      val player1 = "Roger Federer"
      val player2 = "Novak Djokovic"

      if (result.containsPlayer(player1)) {
        val matchPrediction = matchModel.predict(result)

        val playerExProb = if (exPrices.isDefined) 1d / exPrices.get.getPrice(player1) else Double.NaN
        println("%s, %s, %s, %s, prob/exProb: %.2f / %.2f, %s".format(
          matchPrediction.matchResult.tournamentTime, matchPrediction.matchResult.tournamentName,
          player1, matchPrediction.opponentOf(player1), matchPrediction.matchProb(player1), playerExProb, matchPrediction.matchWinner))

        //   println("%s, %s, %.2f, %.2f".format( matchPrediction.matchResult.tournamentTime, matchPrediction.matchResult.tournamentName,
        //       matchPrediction.skillOnServe(player2).m, matchPrediction.skillOnReturn(player2).m))
      }
    }

  }

  private def shuffle(matchResults: Seq[MatchResult]): Seq[MatchResult] = {
    val rand = new Random()
    matchResults.map { r =>

      //      if (true) r
      //      else r.copy(player1 = r.player2, player2 = r.player1, player1Won = !r.player1Won, p1Stats = r.p2Stats, p2Stats = r.p1Stats)
      r
    }
  }
}
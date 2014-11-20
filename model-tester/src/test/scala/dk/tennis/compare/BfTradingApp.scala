package dk.tennis.compare

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import scala.io.Source
import dk.tennis.compare.domain.BfMarket
import dk.tennis.compare.matching.event.GenericEventsMatcher
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
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
import scala.collection.immutable.HashSet
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenpastmatchresults.InferMatchProbGivenPastMatchResults
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresultsloo.InferMatchProbGivenMatchResultsLoo
import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStatsDB
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults

object BfTradingApp extends App with Logging {

  logger.info("Starting BfTradingApp")

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val playersSet = HashSet("Roger Federer", "Andy Roddick")

  val matchResults = shuffle(MatchesLoader.loadMatches(matchesFile, 2008, 2011))
  logger.info("Matches=" + matchResults.size)
  val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
  val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)

  val exPricesModel = ExPricesMatchModel(matchResults, bfMarkets)

  val matchModel = InferMatchProbGivenMatchResultsLoo(matchResults.toIndexedSeq)
  //val matchModel = InferMatchProbGivenPastMatchResults(matchResults.toIndexedSeq)
  // val matchModel = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)

  val head2HeadStatDB = Head2HeadStatsDB(matchResults)
  run()

  def run() {

    //trading simulation

    val trader = Trader()
    val loglik = OnlineAvg()
    val loglikEx = OnlineAvg()

    val players = List("Roger Federer", "NovakDjokovic")
    matchResults.filter(m => true || players.contains(m.player1) && players.contains(m.player2)).foreach { result =>
      val exPrices = exPricesModel.gamePrices(result)

      if (exPrices.isDefined) {
        val matchPrediction = matchModel.predict(result)

        val p1Price = exPrices.get.p1Price
        val p1TrueProb = matchPrediction.matchProb(result.player1)
        val win = result.player1Won
        val outcome = Outcome(p1Price, p1TrueProb, win)
        val headToHeadStat = head2HeadStatDB.getH2HStat(result.player1, result.player2, result.matchTime)
        trader.placeBet(outcome, headToHeadStat)
        println(trader.getBetsNum + "," + trader.getProfit + ":")
        println("expB/actB/expL/actL=%.2f/%.2f/%.2f/%.2f".format(trader.expectedExWinsBack, trader.actualWinsBack, trader.expectedExWinsLay, trader.actualWinsLay))
        val winnerProb = matchPrediction.matchProb(matchPrediction.matchWinner)
        val winnerProbEx = 1d / exPrices.get.getPrice(matchPrediction.matchWinner)

        if (p1Price * p1TrueProb > 1.05 || p1Price * p1TrueProb < 0.95) {

          //   println("%s:%.4f:%.4f".format(result.matchTime + ":" + result.tournamentName + ":" + result.player1 + ":" + result.player2,winnerProb,winnerProbEx))
          loglik.add(log(winnerProb))
          loglikEx.add(log(winnerProbEx))
          println("loglik/loglikEx: %.2f / %.2f".format(loglik.getAvg, loglikEx.getAvg))
        }
      }
    }

    //match analysis
    matchResults.foreach { result =>
      val exPrices = exPricesModel.gamePrices(result)

      val player1 = "Roger Federer"
      val player2 = "Novak Djokovic"

      if (result.containsPlayer(player1) && result.containsPlayer(player2)) {
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
    val rand = new Random(876867765)
    matchResults.map { r =>

      if (rand.nextBoolean) r
      else r.copy(player1 = r.player2, player2 = r.player1, player1Won = !r.player1Won, p1Stats = r.p2Stats, p2Stats = r.p1Stats)

    }
  }
}
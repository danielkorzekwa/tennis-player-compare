package dk.tennis.compare.tester.atp

import java.util.Date
import scala.io.Source
import scala.util.Random
import org.joda.time.DateTime
import org.junit.Test
import org.slf4j.LoggerFactory
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.BfMarket
import dk.tennis.compare.game.tennis.model.ExPriceCompareModel
import dk.tennis.compare.game.tennis.model.ExPricesMatchModel
import dk.tennis.compare.tester.GameModelTester
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2008, 2011)
  val tester = GameModelTester(tournaments)

  @Test def test {

    val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011_less_than_30m_before_kick_off.csv")
    val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)

    val exModel = ExPricesMatchModel(tournaments, bfMarkets)
    val trueSkillExModel = ExPriceCompareModel(exModel)

    val matchFilter = (t: TournamentResult, m: MatchResult) => {

      // log.info(new DateTime(m.timestamp.get).toString() + ". Log likelihood stats = " + tester.getLlhStats());
      new DateTime(t.tournamentTime).getYear() >= 2010
    }

    val modelSummary = tester.run(trueSkillExModel, matchFilter)

    val probs = modelSummary.predictionRecords.filter(r => r.playerAWinnerProb > 0.5)
   

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Prediction error = " + modelSummary.predictionError())
    log.info("players number = " + modelSummary.playersNum)
    log.info("matches number = " + modelSummary.predictionRecords.size)
    val (expectedWins, actualWins) = modelSummary.expectedVsActualWins()
    log.info("Expected/actual wins: %.3f/%s".format(expectedWins, actualWins))

    //    log.info(modelSummary.predictedActualAvgCorrReport)
  }

}
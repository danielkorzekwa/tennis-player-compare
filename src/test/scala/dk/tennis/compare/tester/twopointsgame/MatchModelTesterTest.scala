package dk.tennis.compare.tester.twopointsgame

import org.junit._
import org.junit.Assert._
import org.slf4j.LoggerFactory
import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import dk.atp.api.domain.SurfaceEnum._
import dk.tennis.compare.domain.BfMarket
import scala.io.Source
import dk.atp.api.CSVATPMatchesLoader
import dk.tennis.compare.tester.model.ExPricesMatchModel
import dk.tennis.compare.tester.model.TrueSkillExPriceModel
import dk.tennis.compare.tester.model.TrueSkillMatchModel
import dk.tennis.compare.tester.model.PointStatsMatchModel
import dk.tennis.compare.tester.model.Glicko2MatchModel
import dk.tennis.compare.domain.GameResult
import dk.tennis.compare.tester.GameModelTester

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val gamesFile = "./src/test/resources/two_points_game/game_results_2006_2011.csv"
  val gameResults = Nil

  val tester = GameModelTester(gameResults)

  @Test def test {

    val trueSkillModel = TrueSkillMatchModel()

    val matchFilter = (m: GameResult) => { /** log.info(m.toString); log.info("Log likelihood stats = " + tester.getLlhStats()); */ new DateTime(m.timestamp.get).getYear() >= 2010 }

    val modelSummary = tester.run(trueSkillModel, matchFilter)

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

    //  log.info(modelSummary.predictedActualAvgCorrReport)
  }

}
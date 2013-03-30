package dk.tennis.compare.tester

import org.junit._
import Assert._
import org.slf4j.LoggerFactory
import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tester = MatchModelTester(matchesFile, 2006, 2011)

  @Test def test {

    //    val model = ExPricesMatchModel("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
    //    val model = Glicko2HardMatchModel()
    val model = TrueSkillMatchModel()
    //val model = TrueSkillGlicko2MatchModel()

    val matchFilter = (m: MatchComposite) => { new DateTime(m.tournament.tournamentTime.getTime()).getYear() >= 2010 }
    //    val matchFilter = (m: MatchComposite) => {
    //      new DateTime(m.tournament.tournamentTime.getTime()).getYear() == 2011 &&
    //        m.matchFacts.containsPlayer("Roger Federer") && m.matchFacts.containsPlayer("Novak Djokovic")
    //    }

    val modelSummary = tester.run(model, matchFilter)

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

    //  modelSummary.predictionRecords.foreach(println(_))

    // log.info(modelSummary.predictedActualAvgCorrReport)
  }
}
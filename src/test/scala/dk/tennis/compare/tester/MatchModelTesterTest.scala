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

    val model = Glicko2MatchModel()

    val matchFilter = (m: MatchComposite) => { new DateTime(m.tournament.tournamentTime.getTime()).getYear() >= 2008 }
    //    val matchFilter = (m: MatchComposite) => {
    //      new DateTime(m.tournament.tournamentTime.getTime()).getYear() == 2011 &&
    //        m.matchFacts.containsPlayer("Roger Federer") && m.matchFacts.containsPlayer("Richard Gasquet")
    //    }

    val modelSummary = tester.run(model, matchFilter)

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

    log.info(modelSummary.predictedActualAvgCorrReport)
  }
}
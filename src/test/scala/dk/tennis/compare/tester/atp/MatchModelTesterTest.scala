package dk.tennis.compare.tester.atp

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
import dk.tennis.compare.domain.TennisResult
import dk.tennis.compare.tester.GameModelTester

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val atpMatches = getAtpMatches(matchesFile, 2006, 2011)

  val tester = GameModelTester(atpMatches)

  @Test def test {

    val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
    val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)
    val exModel = ExPricesMatchModel(atpMatches, bfMarkets)

    val glicko2Model = Glicko2MatchModel()
    //    val model = Glicko2HardMatchModel()
    val trueSkillModel = TrueSkillMatchModel()
    //  val model = TrueSkillDBNMatchModel()
    // val model = TrueSkillGlicko2MatchModel()

    val trueSkillExModel = TrueSkillExPriceModel(trueSkillModel, exModel)

    val pointStatModel = PointStatsMatchModel()

    val matchFilter = (m: GameResult) => { /** log.info(m.toString); log.info("Log likelihood stats = " + tester.getLlhStats()); */ new DateTime(m.timestamp.get).getYear() >= 2010 }
    //    val matchFilter = (m: MatchComposite) => {
    //      new DateTime(m.tournament.tournamentTime.getTime()).getYear() == 2011 &&
    //      /**  m.matchFacts.containsPlayer("Roger Federer") && */m.matchFacts.containsPlayer("Novak Djokovic")
    //    }

    val modelSummary = tester.run(trueSkillExModel, matchFilter)

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

    //  log.info(modelSummary.predictedActualAvgCorrReport)
  }

  private def getAtpMatches(matchesFile: String, yearFrom: Int, yearTo: Int): Seq[TennisResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(matchesFile)

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    val gameResults = filteredMatches.map(m =>
      new TennisResult(
        eventName = Some(m.tournament.tournamentName),
        player1 = m.matchFacts.playerAFacts.playerName,
        player2 = m.matchFacts.playerBFacts.playerName,
        player1Win = Some(m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)),
        trueWinProb = None,
        timestamp = Some(m.tournament.tournamentTime.getTime()),
        numOfSets = m.tournament.numOfSet,
        player1ServicePointsWonPct = Some(m.matchFacts.playerAFacts.totalServicePointsWonPct),
        player2ServicePointsWonPct = Some(m.matchFacts.playerBFacts.totalServicePointsWonPct)))

    gameResults
  }
}
package dk.tennis.compare.tester

import org.junit._
import Assert._
import org.slf4j.LoggerFactory
import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.atp.api.domain.SurfaceEnum._
import dk.tennis.compare.domain.Market
import scala.io.Source
import dk.atp.api.CSVATPMatchesLoader
import dk.tennis.compare.tester.model.ExPricesMatchModel
import dk.tennis.compare.tester.model.TrueSkillExPriceModel
import dk.tennis.compare.tester.model.TrueSkillMatchModel
import dk.tennis.compare.tester.model.PointStatsMatchModel
import dk.tennis.compare.tester.model.Glicko2MatchModel

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val atpMatches = getAtpMatches(matchesFile, 2006, 2011)

  val tester = MatchModelTester(atpMatches)

  @Test def test {

    val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
    val bfMarkets = Market.fromCSV(marketDataSource.getLines().drop(1).toList)
    val exModel = ExPricesMatchModel(atpMatches, bfMarkets)

    val glicko2Model = Glicko2MatchModel()
    //    val model = Glicko2HardMatchModel()
    val trueSkillModel = TrueSkillMatchModel()
    //  val model = TrueSkillDBNMatchModel()
    // val model = TrueSkillGlicko2MatchModel()

    val trueSkillExModel = TrueSkillExPriceModel(trueSkillModel, exModel)

    val pointStatModel = PointStatsMatchModel()

    val matchFilter = (m: MatchComposite) => { /** log.info(m.toString); log.info("Log likelihood stats = " + tester.getLlhStats()); */ new DateTime(m.tournament.tournamentTime.getTime()).getYear() >= 2010 }
    //    val matchFilter = (m: MatchComposite) => {
    //      new DateTime(m.tournament.tournamentTime.getTime()).getYear() == 2011 &&
    //      /**  m.matchFacts.containsPlayer("Roger Federer") && */m.matchFacts.containsPlayer("Novak Djokovic")
    //    }

    val modelSummary = tester.run(trueSkillExModel, matchFilter)

    log.info("Log likelihood stats = " + modelSummary.llhStats)
    log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

    // modelSummary.predictionRecords.foreach(println(_))
  //  log.info(modelSummary.predictedActualAvgCorrReport)

    // println(model.getTrueSkillModel.getRatings.toList.sortWith((a, b) => a._2.mean > b._2.mean).take(10).mkString("\n"))

    // import scala.math._
    //  println(GenericTrueSkillMatchProb(pow(25d / 300, 2), pow(25d / 16, 2)).matchProb(TrueSkillRating(4.815584542809833, 0.31059598899951835), TrueSkillRating(4.445835178664356, 0.2986223987719031)))
    //  println(GenericTrueSkillMatchProb(pow(25d / 300, 2), pow(25d / 16, 2)).matchProb(TrueSkillRating(3.432843804299895, 0.3325118664743764), TrueSkillRating(2.4440549265896365, 0.28951337456281023)))
  }

  private def getAtpMatches(matchesFile: String, yearFrom: Int, yearTo: Int): Seq[MatchComposite] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(matchesFile)

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    filteredMatches
  }
}
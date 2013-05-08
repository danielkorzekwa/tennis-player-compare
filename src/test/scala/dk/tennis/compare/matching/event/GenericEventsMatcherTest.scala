package dk.tennis.compare.matching.event

import org.junit._
import Assert._
import dk.atp.api.domain.MatchComposite
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.io.Source
import dk.tennis.compare.domain.Market
import org.joda.time.DateTime

class GenericEventsMatcherTest {

  val atpMarkets = loadAtpMatches()

  val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011.csv")
  val bfMarkets = Market.fromCSV(marketDataSource.getLines().drop(1).toList)

  val eventsMatcher = GenericEventsMatcher(atpMarkets, bfMarkets)

  @Test def test_high_matching_prob {

    val matchingProbs = eventsMatcher.getMatchingProbs.filter { case (key, prob) => prob > 0.9 }

    assertEquals("Memphis TN; U.S.A. ATP World Tour 5002011Group ARegions Morgan Keegan2011", matchingProbs.toList(0)._1)
    assertEquals(0.935, matchingProbs.toList(0)._2, 0.001)

    assertEquals("Los Angeles CA; U.S.A. ATP World Tour 2502010Group AFarmers Classic2010", matchingProbs.toList(5)._1)
    assertEquals(0.961, matchingProbs.toList(5)._2, 0.001)

    assertEquals("Stockholm Sweden ATP World Tour 2502011Group AIF Stockholm Open 20112011", matchingProbs.toList(20)._1)
    assertEquals(0.923, matchingProbs.toList(20)._2, 0.001)

    assertEquals(21, matchingProbs.size, 0.0001)
  }

  private def loadAtpMatches(): Seq[MatchComposite] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

    val matches = (2006 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    filteredMatches
  }
}
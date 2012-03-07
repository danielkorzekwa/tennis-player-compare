package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import dk.atp.api.CSVATPMatchesLoader
import GlickoTennisMatchCompareTest._

object GlickoTennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")

}
class GlickoTennisMatchCompareTest {

  private val matchCompare = new GlickoTennisMatchCompare(atpMatchesLoader, 24, 1500, 350, 9.607, 14)

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.874, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.119, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.874, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.847, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.903, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }
}
package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import dk.atp.api.CSVATPMatchesLoader
import EloTennisMatchCompareTest._

object EloTennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2010_2011.csv")

}
class EloTennisMatchCompareTest {

  private val matchCompare = new EloTennisMatchCompare(atpMatchesLoader,12)

  @Test @Ignore def matchProb_Roger_Federer_vs_Milos_Raonic {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.855, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.144, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.907, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.797, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.8735, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }
}
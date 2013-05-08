package dk.tennis.compare.old.bulkcompare

import org.junit._
import org.junit.Assert._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import dk.atp.api.CSVATPMatchesLoader
import Elo2TennisMatchCompareTest._
import dk.tennis.compare.old.bulkcompare.Elo2TennisMatchCompare

object Elo2TennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")

}
class Elo2TennisMatchCompareTest {

  private val matchCompare = new Elo2TennisMatchCompare(atpMatchesLoader, 24)

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.954, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.045, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.982, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.811, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.963, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }
}
package dk.tennis.compare.old.bulkcompare

import org.junit._
import org.junit.Assert._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import dk.atp.api.CSVATPMatchesLoader
import Glicko2SoftTennisMatchCompareTest._
import dk.tennis.compare.rating.glicko2.CachedGlicko2RatingsLoader
import dk.tennis.compare.old.bulkcompare.Glicko2SoftTennisMatchCompare

object Glicko2SoftTennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")

}
class Glicko2SoftTennisMatchCompareTest {

  private val glickoLoader = CachedGlicko2RatingsLoader(atpMatchesLoader,24,0,100d / 173.7178, 0.06,0.5,14)
  private val matchCompare = new Glicko2SoftTennisMatchCompare(glickoLoader)

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.896, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.103, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.942, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.776, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.932, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }

  @Test(expected = classOf[NoSuchElementException]) def matchProb_Roger_Federer_vs_UnknownPlayer {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "xxx"

    matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012)
  }
}
package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.atp.api.AtpWorldTourApiImpl
import dk.atp.api.tournament.GenericTournamentAtpApi
import org.joda.time.DateTime
import ATPTennisMatchCompareTest._
import dk.atp.api._
import dk.atp.api.tournament._

object ATPTennisMatchCompareTest {
  private var tournamentApi: GenericTournamentAtpApi = new GenericTournamentAtpApi(10000)
   private val genericATPMatchesLoader = new GenericATPMatchesLoader(tournamentApi)
  private val atpMatchesLoader = new CachedATPMatchesLoader(genericATPMatchesLoader)
}

class ATPTennisMatchCompareTest {

  private val matchCompare = new ATPTennisMatchCompare(atpMatchesLoader)

  val marketTime2012 = new DateTime().withYear(2012).toDate()
  val marketTime2011 = new DateTime().withYear(2011).toDate()

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.855, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.144, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.907, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.797, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(Double.NaN, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }

  @Test def matchProb_not_enough_data_for_player_A {
    assertEquals(Double.NaN, matchCompare.matchProb("Milos Raonic", "Roger Federer", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
  }

  @Test def matchProb_not_enough_data_for_player_B {
    assertEquals(Double.NaN, matchCompare.matchProb("Roger Federer", "Milos Raonic", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
  }

  @Test @Ignore def matchProb_various_players_2012 {
    assertEquals(0.489, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.717, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.669, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.087, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
    assertEquals(0.358, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, marketTime2011), 0.001)
    assertEquals(0.4034, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, marketTime2011), 0.001)

    assertEquals(0.761, matchCompare.matchProb("Rafael Nadal", "Roger Federer", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.186, matchCompare.matchProb("Rafael Nadal", "Roger Federer", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.304, matchCompare.matchProb("Rafael Nadal", "Roger Federer", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.747, matchCompare.matchProb("Novak Djokovic", "Roger Federer", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.366, matchCompare.matchProb("Novak Djokovic", "Roger Federer", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.464, matchCompare.matchProb("Novak Djokovic", "Roger Federer", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.783, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.980, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.642, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.534, matchCompare.matchProb("Roger Federer", "Robin Soderling", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.990, matchCompare.matchProb("Roger Federer", "Robin Soderling", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.667, matchCompare.matchProb("Roger Federer", "Robin Soderling", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }

  @Test @Ignore def matchProb_various_players2_2012 {
    assertEquals(0.9999, matchCompare.matchProb("Novak Djokovic", "Michael Russell", CLAY, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9847, matchCompare.matchProb("Novak Djokovic", "Michael Russell", GRASS, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9346, matchCompare.matchProb("Novak Djokovic", "Michael Russell", HARD, THREE_SET_MATCH, marketTime2012), 0.0001)

    assertEquals(0.9995, matchCompare.matchProb("Roger Federer", "Michael Russell", CLAY, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9924, matchCompare.matchProb("Roger Federer", "Michael Russell", GRASS, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9382, matchCompare.matchProb("Roger Federer", "Michael Russell", HARD, THREE_SET_MATCH, marketTime2012), 0.0001)

  }

}
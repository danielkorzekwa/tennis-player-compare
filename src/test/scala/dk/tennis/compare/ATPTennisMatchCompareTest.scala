package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.atp.api.AtpWorldTourApiImpl

class ATPTennisMatchCompareTest {

  private val atpApi = new AtpWorldTourApiImpl()
  private val matchCompare = new ATPTennisMatchCompare(atpApi)

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.768, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.231, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, 2011), 0.001)

    assertEquals(0.820, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, 2011), 0.001)

    assertEquals(0.749, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.8366, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, 2011), 0.001)

    //exception should be thrown here - no data for playerB
    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, 2010), 0.001)

  }

  @Test def matchProb_various_players_2011 {
    assertEquals(0.489, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.717, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.669, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, 2011), 0.001)
    
     assertEquals(0.087, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, 2010), 0.001)
    assertEquals(0.358, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, 2010), 0.001)
    assertEquals(0.4034, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, 2010), 0.001)

    assertEquals(0.761, matchCompare.matchProb("Rafael Nadal", "Roger Federer", CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.186, matchCompare.matchProb("Rafael Nadal", "Roger Federer", GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.304, matchCompare.matchProb("Rafael Nadal", "Roger Federer", HARD, THREE_SET_MATCH, 2011), 0.001)

    assertEquals(0.747, matchCompare.matchProb("Novak Djokovic", "Roger Federer", CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.366, matchCompare.matchProb("Novak Djokovic", "Roger Federer", GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.464, matchCompare.matchProb("Novak Djokovic", "Roger Federer", HARD, THREE_SET_MATCH, 2011), 0.001)

    assertEquals(0.783, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.980, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.642, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", HARD, THREE_SET_MATCH, 2011), 0.001)

    assertEquals(0.534, matchCompare.matchProb("Roger Federer", "Robin Soderling", CLAY, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.990, matchCompare.matchProb("Roger Federer", "Robin Soderling", GRASS, THREE_SET_MATCH, 2011), 0.001)
    assertEquals(0.667, matchCompare.matchProb("Roger Federer", "Robin Soderling", HARD, THREE_SET_MATCH, 2011), 0.001)

    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", HARD, THREE_SET_MATCH, 2011), 0)
  }

}
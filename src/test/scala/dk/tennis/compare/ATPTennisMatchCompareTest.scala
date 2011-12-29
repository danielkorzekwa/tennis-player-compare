package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

class ATPTennisMatchCompareTest {

  private val matchCompare = new ATPTennisMatchCompare()

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos_Raonic"

    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, 2010), 0)
  }

  @Test def matchProb_various_players_2011 {
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Rafael Nadal", "Roger Federer", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Rafael Nadal", "Roger Federer", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Rafael Nadal", "Roger Federer", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Roger Federer", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Roger Federer", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Roger Federer", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Robin Soderling", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Robin Soderling", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Robin Soderling", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Robin Soderling", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Novak Djokovic", "Michael Russell", HARD, THREE_SET_MATCH, 2011), 0)

    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", CLAY, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", GRASS, THREE_SET_MATCH, 2011), 0)
    assertEquals(-1, matchCompare.matchProb("Roger Federer", "Michael Russell", HARD, THREE_SET_MATCH, 2011), 0)
  }

}
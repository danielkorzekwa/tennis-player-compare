package dk.tennis.compare

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import markov._
import MarkovTennisMatchCompareTest._
import org.joda.time.DateTime
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

object MarkovTennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")

}

class MarkovTennisMatchCompareTest {

  def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double =
    playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)
  private val markovRating = new GenericMarkovRating(1, 10, calculateWinProbOnServe)

  private val markovLoader = new CachedMarkovRatingsLoader(markovRating, atpMatchesLoader, 24)

  private val matchCompare = new MarkovTennisMatchCompare(markovLoader)

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.8215, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.1784, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.8751, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.8487, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.8593, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }

  @Test(expected = classOf[NoSuchElementException]) def matchProb_Roger_Federer_vs_UnknownPlayer {

    val marketTime2012 = new DateTime(0).withYear(2012).toDate()

    val playerAFullName = "Roger Federer"
    val playerBFullName = "xxx"

    matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012)
  }

}
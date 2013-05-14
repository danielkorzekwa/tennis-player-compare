package dk.tennis.compare.rating.markov

import org.joda.time.DateTime
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.GRASS
import dk.atp.api.domain.SurfaceEnum.HARD

class CachedMarkovRatingsLoaderTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")

  def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double =
    playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)
  private val markovRating = new GenericMarkovRating(1, 10, calculateWinProbOnServe)

  private val markovLoader = new CachedMarkovRatingsLoader(markovRating, atpMatchesLoader, 24)

  @Test def ratings {

    val ratings = markovLoader.ratings(DateTime.parse("2011-08-12").toDate(), HARD)
    assertEquals(297, ratings.size, 0)

    val timeHardCached = measure(markovLoader.ratings(DateTime.parse("2011-08-12").toDate(), HARD))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeHardCached), timeHardCached < 10)

    val timeGrassNotCached = measure(markovLoader.ratings(DateTime.parse("2011-08-12").toDate(), GRASS))
    assertTrue("Execution time %s was lower than 10ms. Ratings shouldn't be cached now".format(timeGrassNotCached), timeGrassNotCached > 10)

    val timeGrassCached = measure(markovLoader.ratings(DateTime.parse("2011-08-12").toDate(), GRASS))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeGrassCached), timeGrassCached < 10)

    val timeGrassCachedSeptemberNotCached = measure(markovLoader.ratings(DateTime.parse("2011-09-12").toDate(), GRASS))
    assertTrue("Execution time %s was lower than 10ms. Ratings shouldn't be cached now".format(timeGrassCachedSeptemberNotCached), timeGrassCachedSeptemberNotCached > 10)

    val timeGrassCachedSeptemberCached = measure(markovLoader.ratings(DateTime.parse("2011-09-12").toDate(), GRASS))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeGrassCachedSeptemberCached), timeGrassCachedSeptemberCached < 10)

  }

  def measure(code: => Unit): Long = {
    val now = System.currentTimeMillis()
    code
    System.currentTimeMillis() - now
  }
}
package dk.tennis.compare.rating.glicko

import dk.atp.api.CSVATPMatchesLoader
import org.junit._
import Assert._
import org.joda.time._
import dk.atp.api.domain.SurfaceEnum._

class CachedGlickoRatingsLoaderTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2009_2011.csv")
  private val glickoLoader = CachedGlickoRatingsLoader(atpMatchesLoader, 24, 1500, 100, 9.607, 30)

  @Test def ratings {

    val ratings = glickoLoader.ratings(DateTime.parse("2011-08-12").toDate(), HARD)
    assertEquals(297, ratings.size, 0)

    val timeHardCached = measure(glickoLoader.ratings(DateTime.parse("2011-08-12").toDate(), HARD))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeHardCached), timeHardCached < 10)

    val timeGrassNotCached = measure(glickoLoader.ratings(DateTime.parse("2011-08-12").toDate(), GRASS))
    assertTrue("Execution time %s was lower than 10ms. Ratings shouldn't be cached now".format(timeGrassNotCached), timeGrassNotCached > 10)

    val timeGrassCached = measure(glickoLoader.ratings(DateTime.parse("2011-08-12").toDate(), GRASS))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeGrassCached), timeGrassCached < 10)

    val timeGrassCachedSeptemberNotCached = measure(glickoLoader.ratings(DateTime.parse("2011-09-12").toDate(), GRASS))
    assertTrue("Execution time %s was lower than 10ms. Ratings shouldn't be cached now".format(timeGrassCachedSeptemberNotCached), timeGrassCachedSeptemberNotCached > 10)

    val timeGrassCachedSeptemberCached = measure(glickoLoader.ratings(DateTime.parse("2011-09-12").toDate(), GRASS))
    assertTrue("Execution time %s was higher than 10ms. Cache doesn't work".format(timeGrassCachedSeptemberCached), timeGrassCachedSeptemberCached < 10)

  }

  def measure(code: => Unit): Long = {
    val now = System.currentTimeMillis()
    code
    System.currentTimeMillis() - now
  }
}
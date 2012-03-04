package dk.tennis.compare.glicko

import org.junit._
import Assert._
import GlickoRating._

class GenericGlickoRatingTest {

  val glicko = new GenericGlickoRating(1500,350)

  /**Single rating tests.*/

  @Test def calcRatingsSingleResult {
    val ratings = glicko.calcRatings(Result("A", "B", 1) :: Nil)

    assertEquals(1662.212, ratings("A").rating, 0.001)
    assertEquals(290.230, ratings("A").deviation, 0.001)
    assertEquals(1337.787, ratings("B").rating, 0.001)
    assertEquals(290.230, ratings("B").deviation, 0.001)

    assertEquals(0.797, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation),0.001)
  }

  /**On serve and return rating tests.*/
  @Test def calcOnServeReturnRatingsSingleResult {
    val ratings = glicko.calcServeReturnRatings(Result("A", "B", 1) :: Nil)

    assertEquals(1662.212, ratings("A")._1.rating, 0.001)
    assertEquals(290.230, ratings("A")._1.deviation, 0.001)
    assertEquals(1500, ratings("A")._2.rating, 0)
    assertEquals(350, ratings("A")._2.deviation, 0)
    assertEquals(1500, ratings("B")._1.rating, 0)
    assertEquals(350, ratings("B")._1.deviation, 0)
    assertEquals(1337.787, ratings("B")._2.rating, 0.001)
    assertEquals(290.230, ratings("B")._2.deviation, 0.001)

    assertEquals(0.797, glicko.expectedScore(ratings("A")._1.rating, ratings("B")._2.rating, ratings("B")._2.deviation), 0.001)
    assertEquals(0.5, glicko.expectedScore(ratings("B")._1.rating, ratings("A")._2.rating, ratings("A")._2.deviation), 0.001)
  }

  /**Tests for primitive rating functions.*/
  @Test def q = assertEquals(0.0057564, glicko.q, 0.00001)

  @Test def g = {
    assertEquals(0.9955, glicko.g(30), 0.0001)
    assertEquals(0.9531, glicko.g(100), 0.0001)
    assertEquals(0.7242, glicko.g(300), 0.0001)
    assertEquals(1, glicko.g(0), 0.0001)
  }

  @Test def expectedScore {
    assertEquals(0.639, glicko.expectedScore(1500, 1400, 30), 0.001)
    assertEquals(0.432, glicko.expectedScore(1500, 1550, 100), 0.001)
    assertEquals(0.303, glicko.expectedScore(1500, 1700, 300), 0.001)
    assertEquals(0.240, glicko.expectedScore(1500, 1700, 0), 0.001)
  }

  @Test def dSquare {
    assertEquals(132082.427, glicko.dSquare(1500, 1400, 30), 0.001)
  }

  @Test def newRating {
    assertEquals(1563.432, glicko.newRating(Rating(1500, 200), Rating(1400, 30), 1), 0.001)
    assertEquals(1387.492, glicko.newRating(Rating(1500, 200), Rating(1400, 30), 0), 0.001)

    assertEquals(1556.792, glicko.newRating(Rating(1500, 200), Rating(1400, 300), 1), 0.001)
    assertEquals(1413.83, glicko.newRating(Rating(1500, 200), Rating(1400, 300), 0), 0.001)

    assertEquals(1504.097, glicko.newRating(Rating(1500, 50), Rating(1400, 300), 1), 0.001)
    assertEquals(1493.782, glicko.newRating(Rating(1500, 50), Rating(1400, 300), 0), 0.001)

    assertEquals(1501.519, glicko.newRating(Rating(1500, 50), Rating(1400, 300), 0.75), 0.001)
    assertEquals(1498.424, glicko.newRating(Rating(1500, 50), Rating(1400, 300), 0.45), 0.001)
  }

  @Test def newDeviation = {
    assertEquals(175.220, glicko.newDeviation(Rating(1500, 200), Rating(1400, 30)), 0.001)
    assertEquals(179.542, glicko.newDeviation(Rating(1500, 200), Rating(1400, 170)), 0.001)
    assertEquals(183.081, glicko.newDeviation(Rating(1500, 200), Rating(1400, 250)), 0.001)
  }
}
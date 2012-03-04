package dk.tennis.compare.glicko

import org.junit._
import Assert._
import GlickoRating._

class GenericGlickoRatingTest {

  val glicko = new GenericGlickoRating(1500, 350)

  /**Single rating tests.*/

  @Test def calcRatingsSingleResult {
    val ratings = glicko.calcRatings(Result("A", "B", 1) :: Nil)

    assertEquals(1662.212, ratings("A").rating, 0.001)
    assertEquals(290.230, ratings("A").deviation, 0.001)
    assertEquals(1337.787, ratings("B").rating, 0.001)
    assertEquals(290.230, ratings("B").deviation, 0.001)

    assertEquals(0.797, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
  }

  @Test def calcRatingsSingleResult_0_7 {
    val ratings = glicko.calcRatings(Result("A", "B", 0.7) :: Nil)

    assertEquals(1564.884, ratings("A").rating, 0.001)
    assertEquals(290.230, ratings("A").deviation, 0.001)
    assertEquals(1435.115, ratings("B").rating, 0.001)
    assertEquals(290.230, ratings("B").deviation, 0.001)

    assertEquals(0.634, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge {
    val results = (1 to 5).map(i => Result("A", "B", 39d / 52)).toList
    val ratings = glicko.calcRatings(results)

    assertEquals(1613.215, ratings("A").rating, 0.001)
    assertEquals(186.944, ratings("A").deviation, 0.001)
    assertEquals(1386.784, ratings("B").rating, 0.001)
    assertEquals(186.944, ratings("B").deviation, 0.001)

    assertEquals(0.754, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge_low_initial_deviation {
    val glicko = new GenericGlickoRating(1500, 100)
    val results = (1 to 70).map(i => Result("A", "B", 39d / 52)).toList
    val ratings = glicko.calcRatings(results)

    assertEquals(1593.319, ratings("A").rating, 0.001)
    assertEquals(42.426, ratings("A").deviation, 0.001)
    assertEquals(1406.680, ratings("B").rating, 0.001)
    assertEquals(42.426, ratings("B").deviation, 0.001)

    assertEquals(0.743, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
  }

  @Test def calcRatings_for_three_players {
    val results = Result("A", "B", 39d / 52) :: Result("B", "C", 39d / 52) :: Result("C", "A", 39d / 52) :: Nil
    val ratings = glicko.calcRatings(results)

    assertEquals(1459.482, ratings("A").rating, 0.001)
    assertEquals(251.983, ratings("A").deviation, 0.001)
    assertEquals(1500.309, ratings("B").rating, 0.001)
    assertEquals(254.072, ratings("B").deviation, 0.001)
    assertEquals(1502.122, ratings("C").rating, 0.001)
    assertEquals(247.567, ratings("C").deviation, 0.001)

    assertEquals(0.454, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
    assertEquals(0.497, glicko.expectedScore(ratings("B").rating, ratings("C").rating, ratings("C").deviation), 0.001)
    assertEquals(0.451, glicko.expectedScore(ratings("A").rating, ratings("C").rating, ratings("C").deviation), 0.001)
  }

  @Test def calcRatings_for_three_players_2 {
    val results = Result("A", "B", 39d / 52) :: Result("B", "C", 39d / 52) :: Nil
    val ratings = glicko.calcRatings(results)

    assertEquals(0.589, glicko.expectedScore(ratings("A").rating, ratings("B").rating, ratings("B").deviation), 0.001)
    assertEquals(0.619, glicko.expectedScore(ratings("B").rating, ratings("C").rating, ratings("C").deviation), 0.001)
    assertEquals(0.697, glicko.expectedScore(ratings("A").rating, ratings("C").rating, ratings("C").deviation), 0.001)
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

  @Test def calcOnServeReturn_70_games_converge_low_initial_deviation {
    val glicko = new GenericGlickoRating(1500, 100)
    val results = (1 to 70).map(i => Result("A", "B", 39d / 52)).toList
    val ratings = glicko.calcServeReturnRatings(results)

    assertEquals(1593.319, ratings("A")._1.rating, 0.001)
    assertEquals(42.426, ratings("A")._1.deviation, 0.001)
    assertEquals(1500, ratings("A")._2.rating, 0)
    assertEquals(100, ratings("A")._2.deviation, 0)
    assertEquals(1500, ratings("B")._1.rating, 0)
    assertEquals(100, ratings("B")._1.deviation, 0)
    assertEquals(1406.680, ratings("B")._2.rating, 0.001)
    assertEquals(42.426, ratings("B")._2.deviation, 0.001)

    assertEquals(0.743, glicko.expectedScore(ratings("A")._1.rating, ratings("B")._2.rating, ratings("B")._2.deviation), 0.001)
    assertEquals(0.5, glicko.expectedScore(ratings("B")._1.rating, ratings("A")._2.rating, ratings("A")._2.deviation), 0.001)
  }

  @Test def calcOnServeReturn_for_three_players {
    val results = Result("A", "B", 0.75) :: Result("B", "A", 0.71) :: Result("B", "C", 0.68) :: Result("C", "B", 0.62) :: Nil
    val ratings = glicko.calcServeReturnRatings(results)

    assertEquals(0.684, glicko.expectedScore(ratings("A")._1.rating, ratings("B")._2.rating, ratings("B")._2.deviation), 0.001)
    assertEquals(0.667, glicko.expectedScore(ratings("B")._1.rating, ratings("A")._2.rating, ratings("A")._2.deviation), 0.001)

    assertEquals(0.638, glicko.expectedScore(ratings("B")._1.rating, ratings("C")._2.rating, ratings("C")._2.deviation), 0.001)
    assertEquals(0.613, glicko.expectedScore(ratings("C")._1.rating, ratings("B")._2.rating, ratings("B")._2.deviation), 0.001)

    assertEquals(0.623, glicko.expectedScore(ratings("A")._1.rating, ratings("C")._2.rating, ratings("C")._2.deviation), 0.001)
    assertEquals(0.583, glicko.expectedScore(ratings("C")._1.rating, ratings("A")._2.rating, ratings("A")._2.deviation), 0.001)
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
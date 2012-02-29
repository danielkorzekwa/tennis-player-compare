package dk.tennis.compare.elo

import org.junit._
import Assert._
import EloRating._

class GenericEloRatingTest {

  @Test def calcRatingsSingleResult {
    val ratings = GenericEloRating.calcRatings(Result("A", "B", 1, 1) :: Nil)

    assertEquals(1016, ratings("A"), 0)
    assertEquals(984, ratings("B"), 0)
    assertEquals(0.545, GenericEloRating.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge {
    val results = (1 to 70).map(i => Result("A", "B", 39, 52)).toList
    val ratings = GenericEloRating.calcRatings(results)

    assertEquals(1094.923, ratings("A"), 0.001)
    assertEquals(905.076, ratings("B"), 0.001)
    assertEquals(0.748, GenericEloRating.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }
  
  @Test def calcRatingsSingleResult_70_games_converge_then_big_loss {
    val results = (1 to 70).map(i => Result("A", "B", 39, 52)).toList :+ Result("A", "B", 1, 52)
    val ratings = GenericEloRating.calcRatings(results)

    assertEquals(1071.573, ratings("A"), 0.001)
    assertEquals(928.426, ratings("B"), 0.001)
    assertEquals(0.695, GenericEloRating.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge_then_big_losses {
    val results = (1 to 70).map(i => Result("A", "B", 39, 52)).toList
    val results2 = (1 to 70).map(i => Result("A", "B", 13, 52)).toList
    val ratings = GenericEloRating.calcRatings(results ::: results2)

    assertEquals(905.449, ratings("A"), 0.001)
    assertEquals(1094.550, ratings("B"), 0.001)
    assertEquals(0.251, GenericEloRating.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calExpectedScore {
    assertEquals(0.759, GenericEloRating.calcExpectedScore(1200, 1000), 0.001)
    assertEquals(0.759, GenericEloRating.calcExpectedScore(1500, 1300), 0.001)

    assertEquals(0.5, GenericEloRating.calcExpectedScore(1500, 1500), 0.001)

    assertEquals(0.545, GenericEloRating.calcExpectedScore(1016, 984), 0.001)
    assertEquals(0.676, GenericEloRating.calcExpectedScore(1064, 936), 0.001)
  }
}
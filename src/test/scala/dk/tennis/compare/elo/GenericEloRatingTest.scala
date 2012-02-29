package dk.tennis.compare.elo

import org.junit._
import Assert._
import EloRating._

class GenericEloRatingTest {

  @Test def calcRatingsSingleResult {
    val ratings = GenericEloRating.calcRatings(Result("A", "B", 1, 1) :: Nil)

    assertEquals(1016, ratings("A"), 0)
    assertEquals(984, ratings("B"), 0)
  }

  @Test def calcRatingsSingleResult_won_7_played_10 {
    val ratings = GenericEloRating.calcRatings(Result("A", "B", 7, 10) :: Nil)

    assertEquals(1064, ratings("A"), 0)
    assertEquals(936, ratings("B"), 0)
  }

  @Test def calcRatings10Results_Prob_0_7 {

    val won = (1 to 7).map(i => Result("A", "B", 1, 1)).toList
    val lost = (1 to 3).map(i => Result("A", "B", 0, 1)).toList
    val results = lost ::: won
    val ratings = GenericEloRating.calcRatings(results)

    assertEquals(1059.848, ratings("A"), 0.001)
    assertEquals(1030.540, ratings("B"), 0.001)
  }

  @Test def calExpectedScore {
    assertEquals(0.759, GenericEloRating.calcExpectedScore(1200, 1000), 0.001)
    assertEquals(0.759, GenericEloRating.calcExpectedScore(1500, 1300), 0.001)

    assertEquals(0.5, GenericEloRating.calcExpectedScore(1500, 1500), 0.001)

    assertEquals(0.545, GenericEloRating.calcExpectedScore(1016, 984), 0.001)
    assertEquals(0.676, GenericEloRating.calcExpectedScore(1064, 936), 0.001)
  }
}
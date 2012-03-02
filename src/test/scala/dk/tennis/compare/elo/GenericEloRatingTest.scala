package dk.tennis.compare.elo

import org.junit._
import Assert._
import EloRating._

class GenericEloRatingTest {

  val elo = new GenericEloRating()

  /**Single rating tests.*/

  @Test def calcRatingsSingleResult {
    val ratings = elo.calcRatings(Result("A", "B", 1) :: Nil)

    assertEquals(1016, ratings("A"), 0)
    assertEquals(984, ratings("B"), 0)
    assertEquals(0.545, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge {
    val results = (1 to 70).map(i => Result("A", "B", 39d / 52)).toList
    val ratings = elo.calcRatings(results)

    assertEquals(1094.923, ratings("A"), 0.001)
    assertEquals(905.076, ratings("B"), 0.001)
    assertEquals(0.748, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge_then_big_loss {
    val results = (1 to 70).map(i => Result("A", "B", 39d / 52)).toList :+ Result("A", "B", 1d / 52)
    val ratings = elo.calcRatings(results)

    assertEquals(1071.573, ratings("A"), 0.001)
    assertEquals(928.426, ratings("B"), 0.001)
    assertEquals(0.695, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatingsSingleResult_70_games_converge_then_big_losses {
    val results = (1 to 70).map(i => Result("A", "B", 39d / 52)).toList
    val results2 = (1 to 70).map(i => Result("A", "B", 13d / 52)).toList
    val ratings = elo.calcRatings(results ::: results2)

    assertEquals(905.449, ratings("A"), 0.001)
    assertEquals(1094.550, ratings("B"), 0.001)
    assertEquals(0.251, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
  }

  @Test def calcRatings_for_three_players {
    val results = Result("A", "B", 39d / 52) :: Result("B", "C", 39d / 52) :: Result("C", "A", 39d / 52) :: Nil
    val ratings = elo.calcRatings(results)

    assertEquals(999.246, ratings("A"), 0.001)
    assertEquals(1000.368, ratings("B"), 0.001)
    assertEquals(1000.384, ratings("C"), 0.001)
    assertEquals(0.498, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
    assertEquals(0.499, elo.calcExpectedScore(ratings("B"), ratings("C")), 0.001)
    assertEquals(0.498, elo.calcExpectedScore(ratings("A"), ratings("C")), 0.001)
  }

  @Test def calcRatings_for_three_players_transitive {
    val results = (1 to 70).flatMap(i => Result("A", "B", 39d / 52) :: Result("B", "C", 39d / 52) :: Nil).toList
    val ratings = elo.calcRatings(results)

    assertEquals(1178.235, ratings("A"), 0.001)
    assertEquals(1000.227, ratings("B"), 0.001)
    assertEquals(821.537, ratings("C"), 0.001)
    assertEquals(0.735, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
    assertEquals(0.736, elo.calcExpectedScore(ratings("B"), ratings("C")), 0.001)
    assertEquals(0.886, elo.calcExpectedScore(ratings("A"), ratings("C")), 0.001)
  }

  @Test def calcRatings_for_three_players_transitive_converge {
    val results = (1 to 70).flatMap(i => Result("A", "B", 39d / 52) :: Result("B", "C", 39d / 52) :: Nil).toList
    val ratings = elo.calcRatings(results)

    assertEquals(1178.235, ratings("A"), 0.001)
    assertEquals(1000.227, ratings("B"), 0.001)
    assertEquals(821.537, ratings("C"), 0.001)
    assertEquals(0.735, elo.calcExpectedScore(ratings("A"), ratings("B")), 0.001)
    assertEquals(0.736, elo.calcExpectedScore(ratings("B"), ratings("C")), 0.001)
    assertEquals(0.886, elo.calcExpectedScore(ratings("A"), ratings("C")), 0.001)
  }

  /**Ratings on serve and return.*/
  @Test def calcOnServeReturnRatingsSingleResult {
    val ratings = elo.calcServeReturnRatings(Result("A", "B", 1) :: Nil)

    assertEquals(1016, ratings("A")._1, 0)
    assertEquals(1000, ratings("A")._2, 0)
    assertEquals(1000, ratings("B")._1, 0)
    assertEquals(984, ratings("B")._2, 0)
    assertEquals(0.545, elo.calcExpectedScore(ratings("A")._1, ratings("B")._2), 0.001)
    assertEquals(0.5, elo.calcExpectedScore(ratings("B")._1, ratings("A")._2), 0.001)
  }

  @Test def calcOnServeReturnRatingsSingleResult_70_games_converge {
    val results = (1 to 70).flatMap(i => Result("A", "B", 0.75) :: Result("B", "A", 0.68) :: Nil).toList
    val ratings = elo.calcServeReturnRatings(results)

    assertEquals(1094.923, ratings("A")._1, 0.001)
    assertEquals(934.695, ratings("A")._2, 0.001)
    assertEquals(1065.304, ratings("B")._1, 0.001)
    assertEquals(905.076, ratings("B")._2, 0.001)
    assertEquals(0.749, elo.calcExpectedScore(ratings("A")._1, ratings("B")._2), 0.001)
    assertEquals(0.679, elo.calcExpectedScore(ratings("B")._1, ratings("A")._2), 0.001)
  }

  @Test def calcOnServeReturnRatingsSingleResult_70_games_converge_for_3_teams {
    val results = (1 to 70).flatMap(i => Result("A", "B", 0.75) :: Result("B", "A", 0.68) ::
      Result("B", "C", 0.63) :: Result("C", "B", 0.73) :: Nil).toList
    val ratings = elo.calcServeReturnRatings(results)

    assertEquals(1069.003, ratings("A")._1, 0.001)
    assertEquals(944.445, ratings("A")._2, 0.001)
    assertEquals(1074.409, ratings("B")._1, 0.001)
    assertEquals(878.857, ratings("B")._2, 0.001)
    assertEquals(1052.138, ratings("C")._1, 0.001)
    assertEquals(981.145, ratings("C")._2, 0.001)
    assertEquals(0.749, elo.calcExpectedScore(ratings("A")._1, ratings("B")._2), 0.001)
    assertEquals(0.679, elo.calcExpectedScore(ratings("B")._1, ratings("A")._2), 0.001)
    assertEquals(0.631, elo.calcExpectedScore(ratings("B")._1, ratings("C")._2), 0.001)
    assertEquals(0.730, elo.calcExpectedScore(ratings("C")._1, ratings("B")._2), 0.001)
    assertEquals(0.623, elo.calcExpectedScore(ratings("A")._1, ratings("C")._2), 0.001)
    assertEquals(0.650, elo.calcExpectedScore(ratings("C")._1, ratings("A")._2), 0.001)
  }

  /**Tests for primitive rating functions.*/

  @Test def calExpectedScore {
    assertEquals(0.759, elo.calcExpectedScore(1200, 1000), 0.001)
    assertEquals(0.759, elo.calcExpectedScore(1500, 1300), 0.001)
     assertEquals(0.240, elo.calcExpectedScore(1500, 1700), 0.001)

    assertEquals(0.5, elo.calcExpectedScore(1500, 1500), 0.001)

    assertEquals(0.545, elo.calcExpectedScore(1016, 984), 0.001)
    assertEquals(0.676, elo.calcExpectedScore(1064, 936), 0.001)
  }

  @Test def newRating {
    assertEquals(1006.4, elo.newRating(1000, 1, 0.8), 0.001)
    assertEquals(1016, elo.newRating(1000, 1, 0.5), 0.001)
    assertEquals(1025.6, elo.newRating(1000, 1, 0.2), 0.001)

    assertEquals(974.4, elo.newRating(1000, 0, 0.8), 0.001)
    assertEquals(984, elo.newRating(1000, 0, 0.5), 0.001)
    assertEquals(993.6, elo.newRating(1000, 0, 0.2), 0.001)

    assertEquals(996.8, elo.newRating(1000, 0.7, 0.8), 0.001)
    assertEquals(1006.4, elo.newRating(1000, 0.7, 0.5), 0.001)
    assertEquals(1016.0, elo.newRating(1000, 0.7, 0.2), 0.001)
  }
}
package dk.tennis.compare.rating.markov

import java.util.Date

import org.junit.Assert.assertEquals
import org.junit.Test

import dk.tennis.compare.rating.markov.MarkovRating.Rating
import dk.tennis.compare.rating.markov.MarkovRating.Result
import dk.tennisprob.TennisProbCalc.MatchTypeEnum
import dk.tennisprob.TennisProbFormulaCalc

class GenericMarkovRatingTest {

  def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double =
    playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

  private val markovRating = new GenericMarkovRating(1, 10, calculateWinProbOnServe)
  /**
   * Tests for calcRatings
   */

  @Test def calcRatings_no_prior_knowledge_1_won_0_lost {
    val playerARatingOnServe = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (1, 0)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.24074), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.34815), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.41111), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.42593), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.31852), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.25556), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_no_prior_knowledge_1_won_1_lost {
    val playerARatingOnServe = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (1, 1)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.32190), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.34752), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.33058), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.32190), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.34752), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.33058), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_no_prior_knowledge_2_won_0_lost {
    val playerARatingOnServe = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (2, 0)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.17286), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.34868), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.47846), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.51292), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.29426), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.19281), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_no_prior_knowledge_6_won_4_lost {
    val playerARatingOnServe = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (6, 4)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.19533), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.38207), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.42260), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.40630), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.36005), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.23365), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_with_prior_knowledge_1_won_1_lost {
    val playerARatingOnServe = List(Rating(1, 0.11990), Rating(2, 0.36862), Rating(3, 0.51148))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (1, 1)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.11494), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.38150), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.50355), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.30195), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.35101), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.34704), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_with_prior_knowledge_0_won_2_lost {
    val playerARatingOnServe = List(Rating(1, 0.11990), Rating(2, 0.36862), Rating(3, 0.51148))
    val playerBRatingOnReturn = List(Rating(1, 1d / 3), Rating(2, 1d / 3), Rating(3, 1d / 3))
    val (pointsWon, pointsLost) = (0, 2)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.22897), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.40385), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.36718), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.15634), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.34531), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.49836), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_with_prior_knowledge_2_won_1_lost {
    val playerARatingOnServe = List(Rating(1, 0.11990), Rating(2, 0.36862), Rating(3, 0.51148))
    val playerBRatingOnReturn = List(Rating(1, 0.55112), Rating(2, 0.30597), Rating(3, 0.14291))
    val (pointsWon, pointsLost) = (2, 1)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val (playerANewRating, playerBNewRating) = markovRating.calcRatings(playerARatingOnServe, playerBRatingOnReturn, pointsWon, pointsLost)

    assertRating(Rating(1, 0.08901), playerANewRating(0), 0.00001)
    assertRating(Rating(2, 0.37215), playerANewRating(1), 0.00001)
    assertRating(Rating(3, 0.53884), playerANewRating(2), 0.00001)

    assertRating(Rating(1, 0.58934), playerBNewRating(0), 0.00001)
    assertRating(Rating(2, 0.29734), playerBNewRating(1), 0.00001)
    assertRating(Rating(3, 0.11332), playerBNewRating(2), 0.00001)

  }

  @Test def calcRatings_multiple_iterations {
    val initialRating = Rating.generateRatings(1, 100)

    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.04 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))
    val markovRating = new GenericMarkovRating(1, 100, calculateWinProbOnServe)

    var ratingAagainstB = markovRating.calcRatings(initialRating, initialRating, 58, 34)
    var ratingBagainstA = markovRating.calcRatings(initialRating, initialRating, 44, 33)

    val winProbAagainstB = markovRating.calcWinProb(ratingAagainstB._1, ratingAagainstB._2)
    val winProbBagainstA = markovRating.calcWinProb(ratingBagainstA._1, ratingBagainstA._2)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(winProbAagainstB, 1 - winProbBagainstA, MatchTypeEnum.THREE_SET_MATCH)

    assertEquals(0.59315, winProbAagainstB, 0.0001)
    assertEquals(0.5485, winProbBagainstA, 0.0001)
    assertEquals(0.72178, matchProbAGivenB, 0.0001)

  }

  /**
   * Tests for calcWinProb
   */
  @Test def calcWinProb_equal_ratings {
    val playerARatingOnServe = Rating.generateRatings(1, 3)
    val playerBRatingOnReturn = Rating.generateRatings(1, 3)
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val winProb = markovRating.calcWinProb(playerARatingOnServe, playerBRatingOnReturn)
    assertEquals(0.5, winProb, 0.0001)
  }

  @Test def calcWinProb_not_equal_ratings {
    val playerARatingOnServe = List(Rating(1, 0.1), Rating(2, 0.3), Rating(3, 0.6))
    val playerBRatingOnReturn = List(Rating(1, 0.3), Rating(2, 0.5), Rating(3, 0.2))
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = playerARatingOnServe.toDouble / (playerARatingOnServe + playerBRatingOnServe)

    val winProb = markovRating.calcWinProb(playerARatingOnServe, playerBRatingOnReturn)
    assertEquals(0.57067, winProb, 0.00001)

  }

  private def assertRating(expectedRating: Rating, actualRating: Rating, delta: Double) {
    assertEquals(expectedRating.ratingValue, actualRating.ratingValue)
    assertEquals(expectedRating.ratingProb, actualRating.ratingProb, delta)
  }

  /**
   * Tests for calcPlayerRatings
   */
  @Test def calcPlayerRatingsSingleResult {

    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.04 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))
    val markovRating = new GenericMarkovRating(1, 100, calculateWinProbOnServe)

    markovRating.sendResult(Result("A", "B", 1, 0, new Date(0)))
    val ratings = markovRating.getRatings()

    assertEquals(1, ratings("A").ratingOnServe(0).ratingValue)
    assertEquals(0.0034, ratings("A").ratingOnServe(0).ratingProb, 0.0001)
    assertEquals(100, ratings("A").ratingOnServe(99).ratingValue)
    assertEquals(0.0165, ratings("A").ratingOnServe(99).ratingProb, 0.0001)

    assertEquals(1, ratings("A").ratingOnReturn(0).ratingValue)
    assertEquals(0.01, ratings("A").ratingOnReturn(0).ratingProb, 0.0001)
    assertEquals(100, ratings("A").ratingOnReturn(99).ratingValue)
    assertEquals(0.01, ratings("A").ratingOnReturn(99).ratingProb, 0.0001)

    assertEquals(1, ratings("B").ratingOnServe(0).ratingValue)
    assertEquals(0.01, ratings("B").ratingOnServe(0).ratingProb, 0.0001)
    assertEquals(100, ratings("B").ratingOnServe(99).ratingValue)
    assertEquals(0.01, ratings("B").ratingOnServe(99).ratingProb, 0.0001)

    assertEquals(1, ratings("B").ratingOnReturn(0).ratingValue)
    assertEquals(0.0165, ratings("B").ratingOnReturn(0).ratingProb, 0.0001)
    assertEquals(100, ratings("B").ratingOnReturn(99).ratingValue)
    assertEquals(0.0034, ratings("B").ratingOnReturn(99).ratingProb, 0.0001)

    assertEquals(0.6638, markovRating.calcWinProb(ratings("A").ratingOnServe, ratings("B").ratingOnReturn), 0.0001)
    assertEquals(0.5, markovRating.calcWinProb(ratings("B").ratingOnServe, ratings("A").ratingOnReturn), 0.0001)
  }

  @Test def calcPlayerRatingsDoubleResult {

    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.04 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))
    val markovRating = new GenericMarkovRating(1, 100, calculateWinProbOnServe)

    val ratings = markovRating.calcPlayerRatings(Result("A", "B", 64, 36, new Date(0)) :: Result("A", "B", 14, 86, new Date(0)) :: Nil)

    assertEquals(1, ratings("A").ratingOnServe(0).ratingValue)
    assertEquals(0.00012, ratings("A").ratingOnServe(0).ratingProb, 0.0001)
    assertEquals(100, ratings("A").ratingOnServe(99).ratingValue)
    assertEquals(0.0000, ratings("A").ratingOnServe(99).ratingProb, 0.0001)

    assertEquals(1, ratings("A").ratingOnReturn(0).ratingValue)
    assertEquals(0.01, ratings("A").ratingOnReturn(0).ratingProb, 0.0001)
    assertEquals(100, ratings("A").ratingOnReturn(99).ratingValue)
    assertEquals(0.01, ratings("A").ratingOnReturn(99).ratingProb, 0.0001)

    assertEquals(1, ratings("B").ratingOnServe(0).ratingValue)
    assertEquals(0.01, ratings("B").ratingOnServe(0).ratingProb, 0.0001)
    assertEquals(100, ratings("B").ratingOnServe(99).ratingValue)
    assertEquals(0.01, ratings("B").ratingOnServe(99).ratingProb, 0.0001)

    assertEquals(51, ratings("B").ratingOnReturn(50).ratingValue)
    assertEquals(0.0051, ratings("B").ratingOnReturn(50).ratingProb, 0.0001)
    assertEquals(61, ratings("B").ratingOnReturn(60).ratingValue)
    assertEquals(0.0201, ratings("B").ratingOnReturn(60).ratingProb, 0.0001)

    assertEquals(0.1607, markovRating.calcWinProb(ratings("A").ratingOnServe, ratings("B").ratingOnReturn), 0.0001)
    assertEquals(0.5, markovRating.calcWinProb(ratings("B").ratingOnServe, ratings("A").ratingOnReturn), 0.0001)
  }

  @Test def calcPlayerRatings_70_games_converge {
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.04 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))
    val markovRating = new GenericMarkovRating(1, 100, calculateWinProbOnServe)

    val results = (1 to 12).map(i => Result("A", "B", 39, 13, new Date(0))).toList
    val ratings = markovRating.calcPlayerRatings(results)

    assertEquals(0.7496, markovRating.calcWinProb(ratings("A").ratingOnServe, ratings("B").ratingOnReturn), 0.0001)
    assertEquals(0.5, markovRating.calcWinProb(ratings("B").ratingOnServe, ratings("A").ratingOnReturn), 0.0001)
  }

  @Test def calcPlayerRatings_70_games_converge_then_loss {
    def calculateWinProbOnServe(playerARatingOnServe: Int, playerBRatingOnServe: Int): Double = 1 / (1 + Math.pow(Math.E, -0.04 * (playerARatingOnServe.toDouble - playerBRatingOnServe)))
    val markovRating = new GenericMarkovRating(1, 100, calculateWinProbOnServe)

    val results = (1 to 12).map(i => Result("A", "B", 39, 13, new Date(0))).toList ::: List(Result("A", "B", 15, 37, new Date(0)))
    val ratings = markovRating.calcPlayerRatings(results)

    assertEquals(0.5851, markovRating.calcWinProb(ratings("A").ratingOnServe, ratings("B").ratingOnReturn), 0.0001)
    assertEquals(0.5, markovRating.calcWinProb(ratings("B").ratingOnServe, ratings("A").ratingOnReturn), 0.0001)
  }
}
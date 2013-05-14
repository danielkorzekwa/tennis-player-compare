package dk.tennis.compare.game.twopointsgame

import org.junit._
import org.junit.Assert._
import scala.util.Random

class TwoPointsGameTest {

  val player1PointWinProb = (game: TwoPointsGame) => 0.6

  val player1PointWinProbNotIDD = (game: TwoPointsGame) => game match {
    case TwoPointsGame(0, 0) => 0.6
    case TwoPointsGame(1, 0) => 0.45
    case TwoPointsGame(0, 1) => 0.75
    case TwoPointsGame(1, 1) => 0.65
  }

  /**
   * Tests for expectedNumOfPoints
   */

  @Test def expectedNumOfPoints_pointsAreIDD {

    val game = TwoPointsGame(0, 0)
    val prob = game.expectedNumOfWinningPoints(player1PointWinProb)

    assertEquals(1.488, prob._1, 0.00001)
    assertEquals(0.992, prob._2, 0.00001)
  }

  @Test def expectedNumOfPoints_pointsAreNotIDD {

    val game = TwoPointsGame(0, 0)
    val prob = game.expectedNumOfWinningPoints(player1PointWinProbNotIDD)

    assertEquals(1.5795, prob._1, 0.00001)
    assertEquals(1.0505, prob._2, 0.00001)
  }

  /**
   * Tests for gameProb
   */
  @Test def gameProb_pointsAreIDD {

    val game = TwoPointsGame(0, 0)
    val prob = game.gameProb(player1PointWinProb)

    assertEquals(0.648, prob, 0.00001)
  }

  @Test def gameProb_pointsAreNotIDD {

    val game = TwoPointsGame(0, 0)
    val prob = game.gameProb(player1PointWinProbNotIDD)

    assertEquals(0.6795, prob, 0.00001)
  }

  /**
   * Tests for simulate
   *
   */

  @Test def simulate_pointsAreIDD {

    val game = TwoPointsGame(0, 0)
    val gamePoints = game.simulate(player1PointWinProb, new Random(7))

    assertEquals(List(false, false), gamePoints)
  }

  @Test def simulate_pointsAreNotIDD {

    val game = TwoPointsGame(0, 0)

    val gamePoints = game.simulate(player1PointWinProbNotIDD, new Random(7))

    assertEquals(List(false, true, true), gamePoints)
  }

}
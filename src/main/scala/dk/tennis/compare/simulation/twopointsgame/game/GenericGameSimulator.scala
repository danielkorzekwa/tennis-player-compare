package dk.tennis.compare.simulation.twopointsgame.game

import dk.tennis.compare.simulation.twopointsgame.player.Player
import dk.tennis.compare.domain.TwoPointsGameResult
import scala.annotation.tailrec
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.domain.TwoPointsGameResult
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import scala.util.Random

/**
 * Default implementation of GameSimulator.
 *
 * @author Daniel Korzekwa
 */
case class GenericGameSimulator(seed: Long = System.currentTimeMillis()) extends GameSimulator {

  def simulateGames(players: IndexedSeq[Player], yearFrom: Int, yearTo: Int, gamesPerYear: Int,
    perfVariance: (TwoPointsGame) => Tuple2[Double, Double]): Seq[TwoPointsGameResult] = {

    require(players.size >= 2, "Minimum two players are needed for sampling game results")

    val random = new Random(seed)

    val results = (yearFrom to yearTo).flatMap { year =>

      List.fill(gamesPerYear) {

        val playersPair = samplePairOfNumbers(random, players.size)
        val player1 = players(playersPair._1)
        val player2 = players(playersPair._2)

        val player1PointWinProb = (game: TwoPointsGame) => game match {
          case TwoPointsGame(0, 0) => 0.6
          case TwoPointsGame(1, 0) => 0.45
          case TwoPointsGame(0, 1) => 0.75
          case TwoPointsGame(1, 1) => 0.65
        }

        val game = TwoPointsGame(0, 0)
        val (p1ExpectedPointsWon, p2ExpectedPointsWon) = game.expectedNumOfWinningPoints(player1PointWinProb)
        val gamePoints = game.simulate(player1PointWinProb, random)
        val player1Win = gamePoints.last
        val player1WinProb = game.gameProb(player1PointWinProb)
        
        TwoPointsGameResult(
          player1 = player1.name,
          player2 = player2.name,
          player1Win = Option(player1Win),
          trueWinProb = Some(player1WinProb),
          timestamp = Option(new DateTime(year, 1, 1, 1, 1, DateTimeZone.UTC).getMillis()),
          player1ExpectedPointProb = p1ExpectedPointsWon / (p1ExpectedPointsWon + p2ExpectedPointsWon),
          points = gamePoints)
      }
    }
    results
  }

  /**
   * @param maxNumber Sample numbers from 0 to maxNumber-1
   */
  private def samplePairOfNumbers(random: Random, maxNumber: Int): Tuple2[Int, Int] = {
    require(maxNumber >= 2)

    var number1 = 0
    var number2 = 0
    while (number1 == number2) {
      number1 = random.nextInt(maxNumber)
      number2 = random.nextInt(maxNumber)
    }
    (number1, number2)
  }
}
package dk.tennis.compare.game.twopointsgame.simulation

import dk.tennis.compare.game.twopointsgame.TwoPointsGame
import dk.tennis.compare.game.twopointsgame.TwoPointsGameResult

/**
 * Random simulator of two points games. Single game is simulated point by point using MCMC techniques.
 *
 * @author Daniel Korzekwa
 */
trait GameSimulator {

  /**
   * @param players
   * @param yearFrom
   * @param yearTo
   * @param gamesPerYear
   * @param perfVariance This function returns the performance variance given the current state of the game for both players.
   * Tuple2[player1 performance variance, player2 performance variance]
   */
  def simulateGames(players: IndexedSeq[Player], yearFrom: Int, yearTo: Int, gamesPerYear: Int,
    perfVariance: (TwoPointsGame) => Tuple2[Double, Double]): Seq[TwoPointsGameResult]
}
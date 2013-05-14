package dk.tennis.compare.simulation.twopointsgame.game

import dk.tennis.compare.simulation.twopointsgame.player.Player
import dk.tennis.compare.domain.GameResult
import dk.tennis.compare.domain.TwoPointsGameResult

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
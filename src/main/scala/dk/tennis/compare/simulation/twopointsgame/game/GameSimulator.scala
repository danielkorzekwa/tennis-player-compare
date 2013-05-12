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
   * @param player1
   * @param player2
   * @param perfVariance Function that returns player performance variance given the current game state
   */
  def simulateGame(player1: Player, player2: Player, perfVariance: (Game) => Double): TwoPointsGameResult

  /**
   * @param players
   * @param yearFrom
   * @param yearTo
   * @param perfVariance  Function that returns player performance variance given the current game state
   */
  def simulateGames(players: Seq[Player], yearFrom: Int, yearTo: Int, gamesPerYear: Int, perfVariance: (Game) => Double): Seq[TwoPointsGameResult]
}
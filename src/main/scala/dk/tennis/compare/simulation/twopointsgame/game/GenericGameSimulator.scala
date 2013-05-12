package dk.tennis.compare.simulation.twopointsgame.game

import dk.tennis.compare.simulation.twopointsgame.player.Player
import dk.tennis.compare.domain.TwoPointsGameResult

/**
 * Default implementation of GameSimulator.
 *
 * @author Daniel Korzekwa
 */
object GenericGameSimulator extends GameSimulator {

  def simulateGame(player1: Player, player2: Player, perfVariance: (Game) => Double): TwoPointsGameResult = {
    val gameResult = TwoPointsGameResult(
      eventName = Some(""),
      player1 = "",
      player2 = "",
      player1Win = None,
      trueWinProb = None,
      timestamp = None,
      points = Nil)

    gameResult
  }

  def simulateGames(players: Seq[Player], yearFrom: Int, yearTo: Int, gamesPerYear: Int, perfVariance: (Game) => Double): Seq[TwoPointsGameResult] = {
    throw new UnsupportedOperationException("Not implemented yet.")
  }
}
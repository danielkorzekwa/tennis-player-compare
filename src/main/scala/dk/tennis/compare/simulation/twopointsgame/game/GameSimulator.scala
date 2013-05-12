package dk.tennis.compare.simulation.twopointsgame.game

import dk.tennis.compare.simulation.twopointsgame.player.Player
import dk.tennis.compare.domain.GameResult

trait MatchSimulator {

  def simulateGame(player1: Player, player2: Player,perfVariance: (Game) => Double): GameResult

  def simulateGames(players:Seq[Player],yearFrom: Int, yearTo: Int, gamesPerYear: Int,perfVariance: (Game) => Double): Seq[GameResult]
}
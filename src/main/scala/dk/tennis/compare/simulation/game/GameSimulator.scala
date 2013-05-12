package dk.tennis.compare.simulation.game

import dk.tennis.compare.simulation.player.Player

trait MatchSimulator {

  def simulateGame(player1: Player, player2: Player,perfVariance: (Game) => Double): GameResult

  def simulateGames(players:Seq[Player],yearFrom: Int, yearTo: Int, gamesPerYear: Int,perfVariance: (Game) => Double): Seq[GameResult]
}
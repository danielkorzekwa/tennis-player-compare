package dk.tennis.compare.rating.multiskill.matchloader

import java.util.Date

case class Player(playerName: String, opponentName: String, timestamp: Date, pointsWon: Int, skillOnServe: Boolean)

object Player {

  def toPlayers(tournaments: Seq[TournamentResult]): Array[Player] = {

    val players = tournaments.flatMap { t =>

      t.matchResults.flatMap { r =>
        val player1OnServe = Player(r.player1, r.player2, r.matchTime, r.p1Stats.servicePointsWon, skillOnServe = true)
        val player2OnReturn = Player(r.player2, r.player1, r.matchTime, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon, skillOnServe = false)

        val player2OnServe = Player(r.player2, r.player1, r.matchTime, r.p2Stats.servicePointsWon, skillOnServe = true)
        val player1OnReturn = Player(r.player1, r.player2, r.matchTime, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon, skillOnServe = false)

        Array(player1OnServe, player2OnReturn, player2OnServe, player1OnReturn)
      }
    }

    players.toArray
  }

}
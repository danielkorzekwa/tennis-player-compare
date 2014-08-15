package dk.tennis.compare.rating.multiskill.model.perfdiff

import java.util.Date
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.collection.immutable.HashSet

case class Player(playerName: String,opponentName:String, onServe: Boolean, timestamp: Date)

object Player {
  
  def toPlayers(tournaments: Seq[TournamentResult],playersFilter:Set[String]=HashSet()): Array[Player] = {

    val players = tournaments.flatMap { t =>

      t.matchResults.filter(r => playersFilter.isEmpty || (playersFilter.contains(r.player1) && playersFilter.contains(r.player2))).flatMap { r =>
        val player1OnServe = Player(r.player1, r.player2, onServe = true, r.matchTime)
        val player2OnReturn = Player(r.player2, r.player1, onServe = false, r.matchTime)

        val player2OnServe = Player(r.player2, r.player1, onServe = true, r.matchTime)
        val player1OnReturn = Player(r.player1, r.player2, onServe = false, r.matchTime)

        Array(player1OnServe, player2OnReturn, player2OnServe, player1OnReturn)
      }
    }

    players.toArray
  }
}
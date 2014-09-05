package dk.tennis.compare.rating.multiskill.model.perfdiff

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.collection.immutable.HashSet

case class Score(player1: Player, player2: Player, p1PointsWon: Int, p2PointsWon: Int) {

  def hasPlayer(playerName: String): Boolean = {
    (player1.playerName.equals(playerName) || player2.playerName.equals(playerName))
  }
}

object Score {
  def toScores(tournaments: Seq[TournamentResult], playersFilter: Set[String] = HashSet()): Array[Score] = {

    val scores = tournaments.flatMap { t =>

      t.matchResults.filter(r => playersFilter.isEmpty || (playersFilter.contains(r.player1) && playersFilter.contains(r.player2))).flatMap { r =>

        val player1OnServe = Player(r.player1, r.player2, onServe = true, r.matchTime)
        val player2OnReturn = Player(r.player2, r.player1, onServe = false, r.matchTime)

        val player2OnServe = Player(r.player2, r.player1, onServe = true, r.matchTime)
        val player1OnReturn = Player(r.player1, r.player2, onServe = false, r.matchTime)

        val player1OnServeScore = Score(player1OnServe, player2OnReturn, r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon)
        val player2OnServeScore = Score(player2OnServe, player1OnReturn, r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon)
        Array(player1OnServeScore, player2OnServeScore)
      }
    }

    scores.toArray
  }

  def toPlayers(scores: Array[Score]): Array[Player] = {
    scores.flatMap { s =>
      Array(s.player1, s.player2)
    }
  }
}
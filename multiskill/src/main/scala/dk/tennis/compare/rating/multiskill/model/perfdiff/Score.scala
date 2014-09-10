package dk.tennis.compare.rating.multiskill.model.perfdiff

import scala.collection.immutable.HashSet
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

/**
 * @param pointsWon (points won by player 1, points won by player 2)
 */
case class Score(player1: Player, player2: Player, pointsWon: Option[Tuple2[Int, Int]]) {

  def hasPlayer(playerName: String): Boolean = {
    (player1.playerName.equals(playerName) || player2.playerName.equals(playerName))
  }
}

object Score {

  def toScores(r: MatchResult): Array[Score] = {

    val player1OnServe = Player(r.player1, r.player2, onServe = true, r.matchTime)
    val player2OnReturn = Player(r.player2, r.player1, onServe = false, r.matchTime)

    val player2OnServe = Player(r.player2, r.player1, onServe = true, r.matchTime)
    val player1OnReturn = Player(r.player1, r.player2, onServe = false, r.matchTime)

    val player1OnServeScore = Score(player1OnServe, player2OnReturn, Some(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon))
    val player2OnServeScore = Score(player2OnServe, player1OnReturn, Some(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon))
    Array(player1OnServeScore, player2OnServeScore)
  }
  def toScores(matchResults: Seq[MatchResult], playersFilter: Set[String] = HashSet()): Array[Score] = {

    val scores = matchResults
      .filter(r => playersFilter.isEmpty || (playersFilter.contains(r.player1) && playersFilter.contains(r.player2)))
      .flatMap { r => toScores(r) }

    scores.toArray
  }

  def toPlayers(scores: Array[Score]): Array[Player] = {
    scores.flatMap { s =>
      Array(s.player1, s.player2)
    }
  }
}
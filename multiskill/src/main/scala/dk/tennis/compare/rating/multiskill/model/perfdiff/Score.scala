package dk.tennis.compare.rating.multiskill.model.perfdiff

import scala.collection.immutable.HashSet
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import java.util.Date

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

    val numOfSets = r.numOfSets match {
      case 2 => NumOfSets.THREE_SETS
      case 3 => NumOfSets.FIVE_SETS
    }
    
    
    val player1OnServe = Player(r.player1, r.player2, onServe = true, r.matchTime,r.surface,numOfSets)
    val player2OnReturn = Player(r.player2, r.player1, onServe = false, r.matchTime,r.surface,numOfSets)

    val player2OnServe = Player(r.player2, r.player1, onServe = true, new Date(r.matchTime.getTime+1),r.surface,numOfSets)
    val player1OnReturn = Player(r.player1, r.player2, onServe = false, new Date(r.matchTime.getTime+1),r.surface,numOfSets)

    val player1OnServeScore = Score(player1OnServe, player2OnReturn, Some(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon))
    val player2OnServeScore = Score(player2OnServe, player1OnReturn, Some(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon))
    Array(player1OnServeScore, player2OnServeScore)
  }
  def toScores(matchResults: Seq[MatchResult]): Array[Score] = {

    val scores = matchResults  
      .flatMap { r => toScores(r) }

    scores.toArray
  }

  def toPlayers(scores: Array[Score]): Array[Player] = {
    scores.flatMap { s =>
      Array(s.player1, s.player2)
    }
  }
}
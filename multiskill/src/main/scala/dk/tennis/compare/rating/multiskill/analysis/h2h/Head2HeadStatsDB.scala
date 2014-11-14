package dk.tennis.compare.rating.multiskill.analysis.h2h

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import java.util.Date

case class Head2HeadStatsDB(matchResults: Seq[MatchResult]) {

  def getH2HStat(player1: String, player2: String, timestamp: Date): Head2HeadStat = {

    val directMatches = matchResults.filter(m => m.containsPlayer(player1) && m.containsPlayer(player2) && m.matchTime.getTime < timestamp.getTime)
    val (p1Won, p2Won) = directMatches.partition(m => if (player1.equals(m.player1)) m.player1Won else !m.player1Won)
    Head2HeadStat(timestamp, player1, player2, p1Won.size, p2Won.size)
  }
}
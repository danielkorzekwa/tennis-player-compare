package dk.tennis.compare.rating.multiskill.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults
import dk.tennis.compare.rating.multiskill.infer.matchprob.MatchPrediction
import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat
import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStat
import dk.tennis.compare.rating.multiskill.analysis.h2h.Head2HeadStatsDB

object expVersusActualWinProb {

  /**
   * @param matchResults
   * @param playerFilter (playerName,match prediction, h2h stats) => true if player win prob shall be included in expVersusActual statistic
   * Returns (expected match win prob,actual match win prob,match num)
   */
  def apply(matchResults: Seq[MatchResult], playerFilter: (String, MatchPrediction, Head2HeadStat) => Boolean): Tuple3[Double, Double, Int] = {

    val players: Seq[Tuple2[String, MatchPrediction]] = filterPlayers(matchResults, playerFilter)

    var expWins = 0d
    var actualWins = 0
    val playerStats = players.map {
      case (player, matchPrediction) =>

        val winProb = matchPrediction.matchProb(player)
        val actualWin = if (player.equals(matchPrediction.matchWinner)) 1 else 0

        expWins += winProb
        actualWins += actualWin
    }

    (expWins, actualWins, playerStats.size)
  }

  def filterPlayers(matchResults: Seq[MatchResult], playerFilter: (String, MatchPrediction, Head2HeadStat) => Boolean): Seq[Tuple2[String, MatchPrediction]] = {
    val infer = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)
    val h2hStatsDB = Head2HeadStatsDB(matchResults)

    val players: Seq[Tuple2[String, MatchPrediction]] = matchResults.flatMap { m =>
      val matchPrediction = infer.predict(m)

      val filteredPlayers = List(m.player1, m.player2).filter { player =>
        val h2hStat = h2hStatsDB.getH2HStat(player, matchPrediction.opponentOf(player), m.tournamentTime)
       
        playerFilter(player, matchPrediction, h2hStat)
      }.map(p => (p, matchPrediction))

      filteredPlayers
    }

    players
  }
}
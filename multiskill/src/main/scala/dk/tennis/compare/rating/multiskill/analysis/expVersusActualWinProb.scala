package dk.tennis.compare.rating.multiskill.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults
import dk.tennis.compare.rating.multiskill.infer.matchprob.MatchPrediction

object expVersusActualWinProb {

  /**
   * @param matchResults
   * @param playerFilter (playerName,match result) => true if player win prob shall be included in expVersusActual statistic
   * Returns (expected match win prob,actual match win prob,match num)
   */
  def apply(matchResults: Seq[MatchResult], playerFilter: (String, MatchResult) => Boolean): Tuple3[Double, Double, Int] = {

    val infer = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)

    val players: Seq[Tuple2[String, MatchPrediction]] = matchResults.flatMap { m =>
      val matchPrediction = infer.predict(m)
      List((m.player1, matchPrediction), (m.player2, matchPrediction)).filter(p => playerFilter(p._1, p._2.matchResult))
    }

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
}
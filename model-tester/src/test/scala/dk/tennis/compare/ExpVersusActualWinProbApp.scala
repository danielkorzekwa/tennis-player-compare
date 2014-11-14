package dk.tennis.compare

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.analysis.expVersusActualWinProb
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

object ExpVersusActualWinProbApp extends App with Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2012, 2014)
  logger.info("All matches:" + matchResults.size)
  logger.info("All players:" + matchResults.flatMap(m => List(m.player1, m.player2)).distinct.size)

  logger.info("Computing exp versus actual win stats...")
  def playerFilter(player: String, matchResult: MatchResult): Boolean = player.equals("Stan Wawrinka") && matchResult.containsPlayer("Marin Cilic")
  val (expProb, actualProb, matchesNum) = expVersusActualWinProb(matchResults, playerFilter)
  logger.info("DONE")

  println("expWins=%.2f actualWins=%.2f matchesNum=%d".format(expProb, actualProb, matchesNum))
}
package dk.tennis.compare

import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.matchmodel.LooMatchModel
import dk.tennis.compare.rating.multiskill.model.matchmodel.LooMatchModel
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenpastmatchresults.InferMatchProbGivenPastMatchResults

object MatchPredictionApp extends App with Logging {

  val player1 = "Tomas Berdych"
  val player2 = "Marin Cilic"
  val matchTime = new SimpleDateFormat("dd/mm/yyyy").parse("12/11/2014")

  val predictionResult = MatchResult(matchTime, "tournament name", Surface.HARD, player1, player2, matchTime, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2012, 2014)
  logger.info("All matches:" + matchResults.size)
  logger.info("All players:" + matchResults.flatMap(m => List(m.player1, m.player2)).distinct.size)

  val p1Matches = matchResults.filter(m => m.containsPlayer(player1))
  val p2Matches = matchResults.filter(m => m.containsPlayer(player2))

  val matchModel = InferMatchProbGivenPastMatchResults(matchResults.toIndexedSeq)
  //val matchModel = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)
  logger.info("Predicting...")
  val matchPrediction = matchModel.predict(predictionResult)
  logger.info("Done...")
  println("p1 wins prob=" + matchPrediction.matchProb(player1) + ":" + 1d / matchPrediction.matchProb(player1) + ":" + 1d / (1 - matchPrediction.matchProb(player1)))
  println("p1 matches=" + p1Matches.size)
  println("p2 matches=" + p2Matches.size)
  InferSkillGivenPlayer
}
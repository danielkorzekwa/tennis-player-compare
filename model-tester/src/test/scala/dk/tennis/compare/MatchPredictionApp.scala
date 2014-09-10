package dk.tennis.compare

import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.model.matchmodel.PastDataMatchModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats

object MatchPredictionApp extends App {

  val player1 = "Roger Federer"
  val player2 = "Novak Djokovic"
  val matchTime = new SimpleDateFormat("dd/mm/yyyy").parse("10/09/2014")

  val predictionResult = MatchResult(matchTime, "tournament name", player1, player2, matchTime, player1Won = true, numOfSets = 3, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))

  val matchesFile = "match_data_2006_2014_100914.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2014)

  val p1Matches = matchResults.filter(m => m.containsPlayer(player1))
  val p2Matches = matchResults.filter(m => m.containsPlayer(player2))

  val matchModel = PastDataMatchModel(matchResults.toIndexedSeq)
  val matchPrediction = matchModel.predict(predictionResult)
  println("p1 wins prob=" + matchPrediction.matchProb(player1))
  println("p1 matches=" + p1Matches.size)
  println("p2 matches=" + p2Matches.size)
}
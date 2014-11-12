package dk.tennis.compare

import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.model.matchmodel.PastDataMatchModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface

object MatchPredictionApp extends App {
   
 
 
 
  
 
  val player1 = "Stan Wawrinka"
  val player2 = "Marin Cilic"
  val matchTime = new SimpleDateFormat("dd/mm/yyyy").parse("12/11/2014")

  val predictionResult = MatchResult(matchTime, "tournament name", Surface.HARD,player1, player2, matchTime, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2012, 2014)

  val p1Matches = matchResults.filter(m => m.containsPlayer(player1))
  val p2Matches = matchResults.filter(m => m.containsPlayer(player2))

  val matchModel = PastDataMatchModel(matchResults.toIndexedSeq)
  
  println("Predicting...")
  val matchPrediction = matchModel.predict(predictionResult)
  println("p1 wins prob=" + matchPrediction.matchProb(player1) + ":" + 1d/matchPrediction.matchProb(player1) + ":" + 1d/(1-matchPrediction.matchProb(player1)))
  println("p1 matches=" + p1Matches.size)
  println("p2 matches=" + p2Matches.size)
}
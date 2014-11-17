package dk.tennis.compare

import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenpastmatchresults.InferMatchProbGivenPastMatchResults
import java.util.Date

object MatchPredictionApp extends App with Logging {

  val df = new SimpleDateFormat("dd/MM/yyyy")
  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2012, 2014)
  logger.info("All matches:" + matchResults.size)
  logger.info("All players:" + matchResults.flatMap(m => List(m.player1, m.player2)).distinct.size)

  val player1 = "Roger Federer"
  val player2 = "Stan Wawrinka"
  val p1Matches = matchResults.filter(m => m.containsPlayer(player1))
  val p2Matches = matchResults.filter(m => m.containsPlayer(player2))

  val startTime = df.parse("01/01/2012")
  val endTime = df.parse("15/11/2014")
  val MONTH: Long = 1000L * 3600 * 24 * 30

  makePredictions()

  private def makePredictions() {

    logger.info("Predicting...")
    val matchModel = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)

    val predictionsHARD = (startTime.getTime to endTime.getTime by MONTH).map { t =>
      val time = new Date(t)
      val result = MatchResult(time, "tournament name", Surface.HARD, player1, player2, time, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))
      matchModel.predict(result)
    } :+ matchModel.predict(MatchResult(endTime, "tournament name", Surface.HARD, player1, player2, endTime, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0)))

    
     val predictionsCLAY = (startTime.getTime to endTime.getTime by MONTH).map { t =>
      val time = new Date(t)
      val result = MatchResult(time, "tournament name", Surface.CLAY, player1, player2, time, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))
      matchModel.predict(result)
    } :+ matchModel.predict(MatchResult(endTime, "tournament name", Surface.CLAY, player1, player2, endTime, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0)))

     
      val predictionsGRASS = (startTime.getTime to endTime.getTime by MONTH).map { t =>
      val time = new Date(t)
      val result = MatchResult(time, "tournament name", Surface.GRASS, player1, player2, time, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))
      matchModel.predict(result)
    } :+ matchModel.predict(MatchResult(endTime, "tournament name", Surface.GRASS, player1, player2, endTime, player1Won = true, numOfSets = 2, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0)))


    logger.info("Done...")

    
    for(i<- 0 until predictionsHARD.size) {
       println(predictionsHARD(i).matchTime + "," + predictionsHARD(i).matchProb(player1) + "," + predictionsCLAY(i).matchProb(player1) + "," + predictionsGRASS(i).matchProb(player1))
    }
    

    println("\np1 wins prob=" + predictionsHARD.last.matchProb(player1) + ":" + 1d / predictionsHARD.last.matchProb(player1) + ":" + 1d / (1 - predictionsHARD.last.matchProb(player1)))
    println("p1 matches=" + p1Matches.size)
    println("p2 matches=" + p2Matches.size)
  }
}
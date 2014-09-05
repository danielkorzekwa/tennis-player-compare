package dk.tennis.compare.rating.multiskill.scoresim

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShort
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.MatchPrediction
import scala.math._
import breeze.plot.Figure
import breeze.plot.Plot
import breeze.linalg._
import breeze.plot._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong

class scoreSimTest {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2010, 2011)

  val scores: Array[Score] = Score.toScores(tournaments)

  @Test def test {

    val meanFunc = (player: Player) => { if (player.onServe) 5d else 0 }
    val covFunc = PlayerCovFuncShort(Array(log(1),log(300)))
      val covFunc2 = PlayerCovFuncShortLong(Array(log(0.2),log(1),log(1),log(300)))
    val simulScores = scoreSim(scores, meanFunc, covFunc, logPerfStdDev = 2.3)
  

    val p1 = "Roger Federer"
    val p2 = "Novak Djokovic"
    val matchPredictions = MatchPrediction.toMatchPredictions(simulScores.map(s => s.score), simulScores.map(s => s.gamePerfDiff))

    matchPredictions.foreach { matchPrediction =>

      if (matchPrediction.hasPlayer(p1) && matchPrediction.hasPlayer(p2)) {
          println("%s:%.2f:%s".format(matchPrediction.matchTime, matchPrediction.matchProb(p1), matchPrediction.matchWinner()))

//        println("%s:%.2f:%.2f:%d,%d:%s:%s".format(
//          matchPrediction.matchTime,
//          matchPrediction.pointProbOnServe(p1),
//          matchPrediction.scoreOnServe(p1).p1PointsWon.toDouble / (matchPrediction.scoreOnServe(p1).p1PointsWon + matchPrediction.scoreOnServe(p1).p2PointsWon),
//          matchPrediction.scoreOnServe(p1).p1PointsWon,
//          matchPrediction.scoreOnServe(p1).p2PointsWon,
//          matchPrediction.matchWinner(),
//          matchPrediction.opponentOf(p1)))
      }
   //   
    
    }
    
      val skills = simulScores.filter(s => s.score.player1.playerName.equals("Roger Federer"))map(s => s.playerSkill("Roger Federer").m)
    skills.foreach(println(_))
    
      val f = Figure()
val p = f.subplot(0)
p += plot((1 to skills.size).map(i => i.toDouble),skills)
p.ylim(0,10)
System.in.read()
  }
}
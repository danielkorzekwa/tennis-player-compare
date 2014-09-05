package dk.tennis.compare.rating.multiskill.model.perfdiff

import scala.math.log
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import scala.io.Source
import scala.collection.immutable.HashSet
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShort
import dk.tennis.compare.rating.multiskill.scoresim.scoreSim

class GenericPerfDiffTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2011, 2011)

  //  val playersList = HashSet("Roger Federer", "Novak Djokovic")
  val playersList = HashSet[String]()
  val realScores: Array[Score] = Score.toScores(tournaments, playersList)
  
  val scores = simulateScores(realScores)
  val playerNames: Array[String] = Score.toPlayers(scores).map(p => p.playerName).distinct

  logger.info(s"Players by name: ${playerNames.size}")
  logger.info(s"All games (on serve + on return): ${scores.size}")

  //day of match covariance -log of signal standard deviation
  //day of match covariance - log of length scale standard deviation 
  //log of player performance standard deviation
  val (priorSkillOnServe,priorSkillOnReturn) = (5d,0)
  val initialParams = DenseVector(log(1), log(0.1), 2.3)

  val covarianceParams = initialParams.data.dropRight(1)
  val logPerfStdDev = initialParams.data.last

  @Test def test {

    def createPlayersSkillsFactor(players: Array[Player]): SkillsFactor = MultiGPSkillsFactor3(playerSkillMeanPrior, PlayerCovFuncShort(covarianceParams), players)
    val infer = GenericPerfDiff(createPlayersSkillsFactor, logPerfStdDev, scores)
    infer.calibrateModel()
    println(infer.skillsFactor.getPriorSkillsForPlayer("Roger Federer", true).v)

    logger.info("Calculating log likelihood")

    val perfDiffs = infer.inferPerfDiffs()

    val loglik = OutcomeLik.totalLoglik(perfDiffs, scores, score => { score.player1.playerName.equals("Roger Federer"); true })

    println("Total/avg log lik: %.3f/%.4f".format(loglik, loglik / scores.map(s => s.p1PointsWon + s.p2PointsWon).sum))

    val p1 = "Roger Federer"
    val p2 = "Novak Djokovic"

    val matchPredictions = MatchPrediction.toMatchPredictions(scores, perfDiffs)

    val (matchLogLik, matchNum) = MatchPrediction.totalLogLik(matchPredictions)
    println("Total/avg match loglik: %.3f/%.4f".format(matchLogLik, matchLogLik / matchNum))

    matchPredictions.foreach { matchPrediction =>

      if (matchPrediction.hasPlayer(p1) && matchPrediction.hasPlayer(p2)) {
        //  println("%s:%.2f:%s".format(matchPrediction.matchTime, matchPrediction.matchProb(p1), matchPrediction.matchWinner()))

        println("%s:%.2f:%.2f:%d,%d:%s:%s".format(
          matchPrediction.matchTime,
          matchPrediction.pointProbOnServe(p1),
          matchPrediction.scoreOnServe(p1).p1PointsWon.toDouble / (matchPrediction.scoreOnServe(p1).p1PointsWon + matchPrediction.scoreOnServe(p1).p2PointsWon),
          matchPrediction.scoreOnServe(p1).p1PointsWon,
          matchPrediction.scoreOnServe(p1).p2PointsWon,
          matchPrediction.matchWinner(),
          matchPrediction.opponentOf(p1)))
      }

    }

    println("Player skills on serve")
    val p1SkillsMean = infer.calcPosteriorSkillsForPlayer("Roger Federer", true).m
    println(p1SkillsMean)

  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }

  private def simulateScores(realScores: Array[Score]): Array[Score] = {
    val meanFunc = (player: Player) => { if (player.onServe) priorSkillOnServe else priorSkillOnReturn }
    val covFunc = PlayerCovFuncShort(initialParams.data.take(2))
    val simScores = scoreSim(realScores, meanFunc, covFunc, logPerfStdDev = initialParams.data.last)
    val scores = simScores.map(s => s.score)
    scores
  }

}
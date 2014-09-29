package dk.tennis.compare.rating.multiskill.learn

import scala.math.exp
import scala.math.log
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import breeze.linalg.DenseVector
import breeze.optimize.DiffFunction
import breeze.optimize.LBFGS
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.collection.immutable.HashSet
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import dk.tennis.compare.rating.multiskill.scoresim.scoreSim
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.GenericSkillCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponenttype.OpponentType
import scala.util.Random

class LearnTennisParamsTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  val rand = new Random()
  val opponentMap = realScores.map(s => s.player1.playerName).map { playerName =>
    playerName -> OpponentType(playerName, rand.nextBoolean)
  }.toMap

  val (scores, trueLoglik) = simulateScores(realScores)
  // val (scores, trueLoglik) = (realScores,Double.NaN)//simulateScores(realScores)

  val playerNames: Array[String] = Score.toPlayers(scores).map(p => p.playerName).distinct

  logger.info(s"Players by name: ${playerNames.size}")
  logger.info(s"All games (on serve + on return): ${scores.size}")
  @Test def test {

    val skillPriorMeanOnServe = 5
    val skillPriorMeanOnReturn = 0

    val initialParams = DenseVector(log(1), log(0.4), log(0.6),
      log(0.3), log(30), log(1), log(365), 2.3)

    val diffFunction = SkillsDiffFunction(scores, skillPriorMeanOnServe, skillPriorMeanOnReturn, params => OpponentTypeOverTimeCovFunc(params, opponentMap), gradientMask = Some(Array(1, 1, 1, 1, 1, 1,1, 0)), Some(trueLoglik))

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-9)
    val optIters = optimizer.iterations(diffFunction, initialParams).toList
    val newParams = optIters.last.x

    println("Iterations = " + optIters.size)

  }

  /**
   * Returns simulated scores, total loglik
   */
  private def simulateScores(scores: Array[Score]): Tuple2[Array[Score], Double] = {

    val trueParams = DenseVector(log(1), log(0.4), log(0.6),
      log(0.3), log(30), log(1), log(365), 2.3)

    val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)
    val meanFunc = (player: Player) => { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }
    val covFunc = OpponentTypeOverTimeCovFunc(trueParams.data.dropRight(1), opponentMap)
    val simScores = scoreSim(scores, meanFunc, covFunc, logPerfStdDev = trueParams.data.last).map(s => s.score)

    val trueLoglik = SkillsDiffFunction(simScores, trueSkillMeanOnServe, trueSkillMeanOnReturn, params => OpponentTypeOverTimeCovFunc(params, opponentMap)).calculate(trueParams)._1

    (simScores, trueLoglik)
  }

}
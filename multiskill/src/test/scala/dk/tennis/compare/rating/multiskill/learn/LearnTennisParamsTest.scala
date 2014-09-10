package dk.tennis.compare.rating.multiskill.learn

import scala.math.exp
import scala.math.log
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import breeze.linalg.DenseVector
import breeze.optimize.DiffFunction
import breeze.optimize.LBFGS
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiff
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.collection.immutable.HashSet
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong
import dk.tennis.compare.rating.multiskill.scoresim.scoreSim
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShort
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong

class LearnTennisParamsTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2008, 2011)

  // val playersList = HashSet("Roger Federer", "Rafael Nadal", "Novak Djokovic", "Gael Monfils","Lleyton Hewitt","Tomas Berdych","Andreas Seppi","Dominik Hrbaty","Marcos Baghdatis",
  //     "Andy Murray","David Ferrer","Jo-Wilfried Tsonga","Mardy Fish")
  // val playersList = HashSet("Roger Federer", "Novak Djokovic")
  val playersList = HashSet[String]()

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults, playersList)

  //simulate scores
  val trueParams = DenseVector(log(0.2), log(10), log(1), log(300), 2.3)
  val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)
  val meanFunc = (player: Player) => { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }
  val covFunc = PlayerCovFuncShortLong(trueParams.data.dropRight(1))
  val simScores = scoreSim(realScores, meanFunc, covFunc, logPerfStdDev = trueParams.data.last)
 
  val scores = realScores//simScores.map(s => s.score) 
  val playerNames: Array[String] = Score.toPlayers(scores).map(p => p.playerName).distinct

  logger.info(s"Players by name: ${playerNames.size}")
  logger.info(s"All games (on serve + on return): ${scores.size}")
  @Test def test {

    val skillPriorMeanOnServe = 5
    val skillPriorMeanOnReturn = 0

    //short term covariance -log of signal standard deviation
    //short term covariance - log of length scale standard deviation 
    //log of player performance standard deviation
    val initialParams = DenseVector(
     log(1),log(30),log(1), log(300), 2.3)

    //   val trueLoglik = Double.NaN
      val trueLoglik = SkillsDiffFunction(scores, trueSkillMeanOnServe, trueSkillMeanOnReturn, params => PlayerCovFuncShortLong(params)).calculate(trueParams)._1

    val diffFunction = SkillsDiffFunction(scores, skillPriorMeanOnServe, skillPriorMeanOnReturn, params => PlayerCovFuncShortLong(params), gradientMask = Some(Array(1, 1, 1,1,0)), Some(trueLoglik))

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-9)
    val optIters = optimizer.iterations(diffFunction, initialParams).toList
    val newParams = optIters.last.x

    println("Iterations = " + optIters.size)

  }

}
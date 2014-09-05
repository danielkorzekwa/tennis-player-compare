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

class LearnTennisParamsTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2011, 2011)

  // val playersList = HashSet("Roger Federer", "Rafael Nadal", "Novak Djokovic", "Gael Monfils","Lleyton Hewitt","Tomas Berdych","Andreas Seppi","Dominik Hrbaty","Marcos Baghdatis",
  //     "Andy Murray","David Ferrer","Jo-Wilfried Tsonga","Mardy Fish")
  // val playersList = HashSet("Roger Federer", "Novak Djokovic")
  val playersList = HashSet[String]()

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(tournaments, playersList)

  val trueParams = DenseVector(log(0.2),log(1),log(1),log(300), 2.3)
  val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)
  val meanFunc = (player: Player) => { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }
  val covFunc = PlayerCovFuncShortLong(trueParams.data.dropRight(1))
  val simScores = scoreSim(realScores, meanFunc, covFunc, logPerfStdDev = trueParams.data.last)
  val scores = simScores.map(s => s.score)

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
      //-0.04140789666564542, 7.809109984135443, 2.3) best real data 2008-2011
      //-0.10923338514723163, 8.451366872708295, 2.3) best learning with no clearing correlation 2008-2011
      log(1), log(300), 2.3)
    //   0.6990005656509222, 3.4355359673993773, 2.3)
    //2.3695206503159207, 2.8862409587827202, 2.3)
    //  -1.1615088954858415, 10.1093659496799362, 2.3)
    //    11.727651128628754, 9.679373197510948, 2.3)

    val trueLoglik = SkillsDiffFunction(scores, trueSkillMeanOnServe, trueSkillMeanOnReturn).calculate(trueParams)._1
    //   val trueLoglik = Double.NaN
    val diffFunction = SkillsDiffFunction(scores, skillPriorMeanOnServe, skillPriorMeanOnReturn, gradientMask = Some(Array(1, 1, 0)), Some(trueLoglik))

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-9)
    val optIters = optimizer.iterations(diffFunction, initialParams).toList
    val newParams = optIters.last.x

    println("Iterations = " + optIters.size)

  }

}
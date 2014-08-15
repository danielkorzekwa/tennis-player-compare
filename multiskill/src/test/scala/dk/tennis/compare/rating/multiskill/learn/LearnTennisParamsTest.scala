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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SingleGPSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.MultiGPSkillsFactor
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.collection.immutable.HashSet
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph

class LearnTennisParamsTest extends Logging {

  //    val players = (1 to 5).flatMap(i => GameTestData.players).toArray
  //    val scores = (1 to 5).flatMap(i => GameTestData.scores).toArray

  //  val players = GameTestData.getGamePlayers(5)
  //  val scores = GameTestData.getScores(5)

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2011, 2011) //.take(50)

  // val playersList = HashSet("Roger Federer", "Rafael Nadal", "Novak Djokovic", "Gael Monfils","Lleyton Hewitt","Tomas Berdych","Andreas Seppi","Dominik Hrbaty","Marcos Baghdatis",
  //     "Andy Murray","David Ferrer","Jo-Wilfried Tsonga","Mardy Fish")

  val playersList = HashSet[String]()

  val players: Array[Player] = Player.toPlayers(tournaments, playersList)
  val playerNames: Array[String] = Player.toPlayers(tournaments, playersList).map(p => p.playerName).distinct
  val scores: Array[Score] = Score.toScores(tournaments, playersList)
  logger.info(s"Players by name: ${playerNames.size}")
  logger.info(s"All players in all games: ${players.size}")
  logger.info(s"All games (on serve + on return): ${scores.size}")
  @Test def test {

    //day of match covariance -log of signal standard deviation
    //day of match covariance - log of length scale standard deviation 
    //log of player performance standard deviation
    val initialParams = DenseVector(-0.614984567030472, 2.3112331737760465, 2.329077490665681)
    //  val initialParams = DenseVector(log(1), log(10), log(sqrt(100)))
    val diffFunction = SkillsDiffFunction()

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 1000, m = 6, tolerance = 1.0E-6)
    val optIters = optimizer.iterations(diffFunction, initialParams).toList
    val newParams = optIters.last.x

    println("Iterations = " + optIters.size)
    println("\nInitial (loglik, derivaties) = " + diffFunction.calculate(initialParams))
    println("\nFinal (loglik, derivaties) = " + diffFunction.calculate(newParams))
    println("\nInitial/new parameters = " + initialParams + "/" + newParams)

  }

  case class SkillsDiffFunction extends DiffFunction[DenseVector[Double]] {

    private var skillPriorMeanOnServe = 4.47
    private var skillPriorMeanOnReturn = -0.59

    def calculate(params: DenseVector[Double]): (Double, DenseVector[Double]) = {

      println("params: %s, priorMean(serve/return): %.2f / %.2f".format(params.toString, skillPriorMeanOnServe, skillPriorMeanOnReturn))

      val logSf = params(0)
      val logEll = params(1)
      val logPerfStdDev = params(2)

      //   val skillsFactor = SingleGPSkillsFactor(ell, players)

      val skillsFactor = MultiGPSkillsFactor(logSf, logEll, playerSkillMeanPrior, players)
      val skillsFactorGraph = SkillsFactorGraph(scores, logPerfStdDev, skillsFactor)
      try {
        val gp = GenericPerfDiff(skillsFactorGraph, logPerfStdDev, scores, threshold = 0.6)

        val (perfDiffs, perfDiffsMeanD, perfDiffsVarD) = gp.inferPerfDiffsWithD()

        val f = -OutcomeLik.totalLoglik(perfDiffs, scores)

        val df = (0 until perfDiffsMeanD.numCols).map { i =>
          val meanD = perfDiffsMeanD.column(i)
          val varD = perfDiffsVarD.column(i)

          val partialDf = OutcomeLik.totalLoglikD(perfDiffs, meanD.toArray, varD.toArray, scores)
          partialDf

        }.toArray

        //learning of the skills mean function
        val playerSkillMarginals: Array[Double] = skillsFactorGraph.getPlayerSkillsMarginalMean().toArray
        val (newPriorSkillMeanOnServe, newPriorSkillMeanOnReturn) = learnSkillMeanFunction(players, playerSkillMarginals)
        skillPriorMeanOnServe = newPriorSkillMeanOnServe
        skillPriorMeanOnReturn = newPriorSkillMeanOnReturn

        println("loglik: %.2f, d: %s,".format(f, df.toList))

        (f, DenseVector(df) * (-1d))
      } catch {
        case e: Exception => {
          logger.warn("Log lik computing error")
          (Double.NaN, params.map(v => Double.NaN))
        }
      }

    }

    private def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) skillPriorMeanOnServe else skillPriorMeanOnReturn
    }
  }

  private def learnSkillMeanFunction(players: Seq[Player], playerSkillMarginals: Array[Double]): Tuple2[Double, Double] = {

    val marginalsOnServe = players.zip(playerSkillMarginals).filter(p => p._1.onServe).map(_._2)
    val marginalsOnReturn = players.zip(playerSkillMarginals).filter(p => !p._1.onServe).map(_._2)

    val meanOnServe = marginalsOnServe.sum / marginalsOnServe.size
    val meanOnReturn = marginalsOnReturn.sum / marginalsOnReturn.size
    (meanOnServe, meanOnReturn)
  }

}
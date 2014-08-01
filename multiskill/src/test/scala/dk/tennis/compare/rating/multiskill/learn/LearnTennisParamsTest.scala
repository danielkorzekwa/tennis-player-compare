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

class LearnTennisParamsTest extends Logging {

  val players = (1 to 5).flatMap(i => GameTestData.players).toArray
  val scores = (1 to 5).flatMap(i => GameTestData.scores).toArray

  logger.info(s"All players in all games: ${players.size}")

  @Test def test {

    //log of length scale
    val initialParams = DenseVector(log(10))
    val diffFunction = SkillsDiffFunction()

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 1000, m = 6, tolerance = 1.0E-6)
    val optIters = optimizer.iterations(diffFunction, initialParams).toList
    val newParams = optIters.last.x

    println("Iterations = " + optIters.size)
    println("\nInitial (loglik, derivaties) = " + diffFunction.calculate(initialParams))
    println("\nFinal (loglik, derivaties) = " + diffFunction.calculate(newParams))
    println("\nInitial/new parameters = " + initialParams.map(v => exp(v)) + "/" + newParams.map(v => exp(v)))

  }

  case class SkillsDiffFunction extends DiffFunction[DenseVector[Double]] {

    private val perfVar = 100

    def calculate(params: DenseVector[Double]): (Double, DenseVector[Double]) = {

      val ell = params(0)
      val skillsFactor = SingleGPSkillsFactor(ell, players)
      val gp = GenericPerfDiff(skillsFactor, perfVar, perfVar, scores, threshold = 0.6)

      val (perfDiffs, perfDiffsMeanD, perfDiffsVarD) = gp.inferPerfDiffs()

      val f = -OutcomeLik.totalLoglik(perfDiffs, scores)

      val df = (0 until perfDiffsMeanD.numCols).map { i =>
        val meanD = perfDiffsMeanD.column(i)
        val varD = perfDiffsVarD.column(i)

        val partialDf = OutcomeLik.totalLoglikD(perfDiffs, meanD.toArray, varD.toArray, scores)
        partialDf

      }.toArray

      (f, DenseVector(df) * (-1d))
    }

  }

}
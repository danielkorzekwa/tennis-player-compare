package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import java.util.Date
import breeze.linalg.DenseVector
import scala.math._
import breeze.optimize.LBFGS
import breeze.optimize.DiffFunction
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.gpskill.Player
import dk.tennis.compare.rating.multiskill.model.gpskill.naive.NaiveGPSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.multi.MultiGPSkills

class LearnTennisParamsTest {

  val players = (1 to 5).flatMap(i => GameTestData.players).toArray
  val scores = (1 to 5).flatMap(i => GameTestData.scores).toArray

  @Test def test {

    //log of length scale
    val initialParams = DenseVector(log(10))
    val diffFunction = SkillsDiffFunction()

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-6)
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

      // val gp = NaiveGPSkills(params.toArray, perfVar, perfVar, players, scores, threshold = 0.6)
      val gp = MultiGPSkills(params.toArray, perfVar, perfVar, players, scores, threshold = 0.6)

      val f = -gp.loglik()
      val df = gp.loglikD()
      (f, DenseVector(df) * (-1d))
    }

  }
}
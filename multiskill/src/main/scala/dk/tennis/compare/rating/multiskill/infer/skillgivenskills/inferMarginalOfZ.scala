package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.Gaussian
import dk.bayes._
import dk.bayes.dsl.infer
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
object inferMarginalOfZ {

  def apply(xPriorMean: DenseVector[Double], xPriorVarInv: DenseMatrix[Double], xMean: DenseVector[Double], Kxx:  DenseMatrix[Double], zPriorMean: DenseVector[Double], Kzz:  DenseMatrix[Double], Kzx:  DenseMatrix[Double]): Gaussian = {

    val x = dsl.variable.Gaussian(xMean, Kxx)

    val A = Kzx * xPriorVarInv
    val b = zPriorMean - A * xPriorMean
    val Kz_x = Kzz - Kzx * xPriorVarInv * Kzx.t

    val y = dsl.variable.Gaussian(A, x, b, Kz_x)

    val marginal = infer(y)
    Gaussian(marginal.m(0), marginal.v(0,0))
  }

}
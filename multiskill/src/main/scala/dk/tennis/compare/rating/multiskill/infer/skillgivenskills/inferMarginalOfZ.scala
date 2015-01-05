package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.Gaussian
import dk.bayes._
import dk.bayes.dsl.infer
object inferMarginalOfZ {

  def apply(xPriorMean: Matrix, xPriorVarInv: Matrix, xMean: Matrix, Kxx: Matrix, zPriorMean: Matrix, Kzz: Matrix, Kzx: Matrix): Gaussian = {

    val x = dsl.variable.Gaussian(xMean, Kxx)

    val A = Kzx * xPriorVarInv
    val b = zPriorMean - A * xPriorMean
    val Kz_x = Kzz - Kzx * xPriorVarInv * Kzx.t

    val y = dsl.variable.Gaussian(A, x, b, Kz_x)

    val marginal = infer(y)
    Gaussian(marginal.m(0), marginal.v(0))
  }

}
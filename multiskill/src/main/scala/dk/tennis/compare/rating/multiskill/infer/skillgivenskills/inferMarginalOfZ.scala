package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.Gaussian

object inferMarginalOfZ {

  def apply(xPriorMean: Matrix, xPriorVarInv: Matrix, xMean: Matrix, Kxx: Matrix, zPriorMean: Matrix, Kzz: Matrix, Kzx: Matrix): Gaussian = {

    val Kxz = Kzx.t
    val A = Kzx * xPriorVarInv

    val Kz_x = Kzz - Kzx * xPriorVarInv * Kxz

    /** RUN INFERENCE - Compute p(z) = integral of p(x)*p(z|x)dx*/
    val skillMean = zPriorMean + A * (xMean - xPriorMean)
    val skillVar = Kz_x + A * Kxx * A.t

    Gaussian(skillMean(0), skillVar(0))
  }

}
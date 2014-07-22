package dk.tennis.compare.rating.multiskill.model.outcomelik

import dk.bayes.math.gaussian.Gaussian
import scala.math._
import Gaussian._

object OutcomeLik {

  def loglik(skillDiff: Gaussian, win: Boolean): Double = {

    val winProb = 1 - stdCdf(-skillDiff.m / sqrt(skillDiff.v))
    win match {
      case true => log(winProb)
      case false => log1p(-winProb)
    }
  }

  /**
   * Returns partial derivative of log likelihood with respect to some parameter theta
   *
   * @param skillDiff
   * @param win
   * @param muD Partial derivative of the mean of skills difference
   * @param varD Partial derivative of the variance of skills difference
   */
  def loglikD(skillDiff: Gaussian, win: Boolean, muD: Double, varD: Double): Double = {

    val m = skillDiff.m
    val v = skillDiff.v

    val x = -m / sqrt(v)

    val xD = -(muD / sqrt(v) - 0.5 * m * (1d / pow(v, 3d / 2) * varD))

    //derivative of cdfVal
    val cdfValD = stdPdf(x) * xD

    win match {
      case true => -1 * (1d / (1 - stdCdf(x))) * cdfValD
      case false => (1d / stdCdf(x)) * cdfValD
    }
  }
}
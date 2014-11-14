package dk.tennis.compare.rating.multiskill.infer.outcome

import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.math._
import Gaussian._

object InferOutcomeGivenPerfDiff {

   def totalLoglik(perfDiffs: Array[Gaussian], scores: Array[Score], filter: (Score) => Boolean = { score => true }): Double = {

    val logliks = scores.zip(perfDiffs).filter { case (score, perfDiff) => filter(score) }.map {
      case (score, perfDiff) =>
        val loglik = score.pointsWon.get._1 * InferOutcomeGivenPerfDiff.loglik(perfDiff, true) + score.pointsWon.get._2 * InferOutcomeGivenPerfDiff.loglik(perfDiff, false)
      
        loglik
    }
    logliks.sum
  }

  def loglik(perfDiff: Gaussian, win: Boolean): Double = {

    val winProb = 1 - stdCdf(-perfDiff.m / sqrt(perfDiff.v))
    win match {
      case true => log(winProb)
      case false => log1p(-winProb)
    }
  }

  /**
   * @param perfDiffsMuD Partial derivative for the mean of the game performance difference with respect to some hyper parameter
   * @param perfDiffsVaRD Partial derivative for the variance of the game performance difference with respect to some hyper parameter
   */
  def totalLoglikD(perfDiffs: Array[Gaussian], perfDiffsMuD: Array[Double], perfDiffsVarD: Array[Double], scores: Array[Score]): Double = {

    val totalLogLikD = (0 until perfDiffs.size).map { i =>
      val perfDiff = perfDiffs(i)
      val muD = perfDiffsMuD(i)
      val varD = perfDiffsVarD(i)
      val score = scores(i)

      val loglikD = score.pointsWon.get._1 * InferOutcomeGivenPerfDiff.loglikD(perfDiff, true, muD, varD) + score.pointsWon.get._2 * InferOutcomeGivenPerfDiff.loglikD(perfDiff, false, muD, varD)
      loglikD
    }.sum
    totalLogLikD
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
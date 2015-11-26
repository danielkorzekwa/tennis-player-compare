package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.linear._
import dk.bayes.math.gaussian.MultivariateGaussian
import scala.math._
import dk.tennis.compare.rating.multiskill.infer.outcome.InferOutcomeGivenPerfDiff
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import breeze.linalg.DenseMatrix
import breeze.linalg.Matrix
import breeze.linalg.DenseVector

case class GenericPointCorModel(perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends PointCorModel {

  def skillMarginals(directSkills: DenseCanonicalGaussian, p1Wins: Boolean): DenseCanonicalGaussian = {

    //factors
    val perf_factor = DenseCanonicalGaussian(a = new DenseMatrix(2, 2, Array(1d, 0, 0, 1)).t, b = DenseVector(0.0, 0.0), v = new DenseMatrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)).t)
    val perf_diff_factor = DenseCanonicalGaussian(DenseMatrix(1d, -1d).t, 0, 1e-12)

    //message passing
    val perf_factor_down = (directSkills.extend(4, 0) * perf_factor).marginal(2, 3)

    val diff_factor_down = (perf_factor_down.extend(3, 0) * perf_diff_factor).marginal(2).toGaussian

    val outcome_factor_up = (diff_factor_down.truncate(0, p1Wins)) / diff_factor_down

    val diff_factor_up = (perf_diff_factor * DenseCanonicalGaussian(outcome_factor_up.m, outcome_factor_up.v).extend(3, 2)).marginalise(2)

    val perf_factor_up = (perf_factor * diff_factor_up.extend(4, 2)).marginalise(3).marginalise(2)

    val skillsMarginal = perf_factor_up * directSkills
    skillsMarginal
  }

  def pointProb(directSkills: DenseCanonicalGaussian): Double = {

    val a = DenseMatrix(1d, -1d).t
    val v = new DenseMatrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)).t

    val diff_factor_down = MultivariateGaussian((a * directSkills.mean), (a * (directSkills.variance + v) * a.t)).toGaussian

    exp(InferOutcomeGivenPerfDiff.loglik(diff_factor_down, true))
  }

}
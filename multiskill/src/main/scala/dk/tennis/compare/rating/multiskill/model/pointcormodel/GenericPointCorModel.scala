package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._

case class GenericPointCorModel(perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends PointCorModel {

  def skillMarginals(directSkills: CanonicalGaussian, p1Wins: Boolean): CanonicalGaussian = {

    //factors
    val perf_factor = CanonicalGaussian(a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)))
    val perf_diff_factor = CanonicalGaussian(Matrix(1d, -1d), 0, 1e-12)

    //message passing
    val perf_to_diff = (directSkills.extend(4, 0) * perf_factor).marginal(2, 3)

    val diff_to_outcome = (perf_to_diff.extend(3, 0) * perf_diff_factor).marginal(2).toGaussian
    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    val diff_to_perf = (perf_diff_factor * CanonicalGaussian(outcome_to_diff.m, outcome_to_diff.v).extend(3, 2)).marginalise(2)

    val perf_to_skill = (perf_factor * diff_to_perf.extend(4, 2)).marginal(0,1)

    val skillsMarginal = perf_to_skill * directSkills
    skillsMarginal
  }

  def pointProb(directSkills: CanonicalGaussian): Double = {

    val perf_factor = CanonicalGaussian(a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)))

    val perf_diff_factor = CanonicalGaussian(Matrix(1d, -1d), 0, 1e-12)

    val perf_to_diff = (directSkills.extend(4, 0) * perf_factor).marginal(2, 3)

    val diff_to_outcome = (perf_to_diff.extend(3, 0) * perf_diff_factor).marginal(2).toGaussian

    val pointProb = 1 - diff_to_outcome.cdf(0)

    pointProb
  }

}
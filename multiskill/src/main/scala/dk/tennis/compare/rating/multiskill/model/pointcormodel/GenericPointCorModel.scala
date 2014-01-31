package dk.tennis.compare.rating.multiskill.model.pointcormodel

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear._

case class GenericPointCorModel(perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends PointCorModel {

  def skillMarginals(playerSkillOnServe: PlayerSkill, playerSkillOnReturn: PlayerSkill, p1Wins: Boolean, skillCovariance: Double): Tuple3[PlayerSkill, PlayerSkill, Double] = {

    //factors
    val prior_skill_mean = Matrix(2, 1, Array(playerSkillOnServe.mean, playerSkillOnReturn.mean))
    val prior_skill_var = Matrix(2, 2, Array(playerSkillOnServe.variance, skillCovariance, skillCovariance, playerSkillOnReturn.variance))
    val prior_skill_factor = CanonicalGaussian(Array(1, 2), prior_skill_mean, prior_skill_var)

    val perf_factor = CanonicalGaussian(Array(1, 2, 3, 4), a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)))

    val perf_diff_factor = CanonicalGaussian(Array(3, 4, 5), Matrix(1d, -1d), 0, 1e-12)

    //message passing
    val perf_to_diff = (prior_skill_factor * perf_factor).marginalise(1).marginalise(2)

    val diff_to_outcome = (perf_to_diff * perf_diff_factor).marginal(5).toGaussian
    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    val diff_to_perf = (perf_diff_factor * CanonicalGaussian(5, outcome_to_diff.m, outcome_to_diff.v)).marginalise(5)

    val perf_to_skill = (perf_factor * diff_to_perf).marginalise(3).marginalise(4)

    val skillsMarginal = perf_to_skill * prior_skill_factor
    val (skillsMarginalMean, skillsMarginalVar) = skillsMarginal.getMeanAndVariance()

    val p1MarginalFactor = PlayerSkill(skillsMarginalMean(0), skillsMarginalVar(0, 0))
    val p2MarginalFactor = PlayerSkill(skillsMarginalMean(1), skillsMarginalVar(1, 1))
    (p1MarginalFactor, p2MarginalFactor, skillsMarginalVar(0, 1))
  }

  def pointProb(playerSkillOnServe: PlayerSkill, playerSkillOnReturn: PlayerSkill, skillCovariance: Double): Double = {
    val prior_skill_mean = Matrix(2, 1, Array(playerSkillOnServe.mean, playerSkillOnReturn.mean))
    val prior_skill_var = Matrix(2, 2, Array(playerSkillOnServe.variance, skillCovariance, skillCovariance, playerSkillOnReturn.variance))
    val prior_skill_factor = CanonicalGaussian(Array(1, 2), prior_skill_mean, prior_skill_var)

    val perf_factor = CanonicalGaussian(Array(1, 2, 3, 4), a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(perfVarianceOnServe, 0, 0, perfVarianceOnReturn)))

    val perf_diff_factor = CanonicalGaussian(Array(3, 4, 5), Matrix(1d, -1d), 0, 1e-12)

    val perf_to_diff = (prior_skill_factor * perf_factor).marginalise(1).marginalise(2)

    val diff_to_outcome = (perf_to_diff * perf_diff_factor).marginal(5).toGaussian

    val pointProb = 1 - diff_to_outcome.cdf(0)

    pointProb
  }

}
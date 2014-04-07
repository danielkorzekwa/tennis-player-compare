package dk.tennis.compare.rating.multiskill.model.pointmodel

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.LinearGaussian
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.LinearGaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.GaussianFactor

case class GenericPointModel(perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends PointModel {

  def skillMarginals(playerSkillOnServe: Gaussian, playerSkillOnReturn: Gaussian, p1Wins: Boolean): Tuple2[Gaussian, Gaussian] = {

    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.m, playerSkillOnServe.v + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.m, playerSkillOnReturn.v + perfVarianceOnReturn)

    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    var diff_to_perf_on_serve = outcome_to_diff + perf_on_return_to_diff
    val diff_to_perf_on_return = perf_on_serve_to_diff - outcome_to_diff

    var perf_to_skill_on_serve = Gaussian(diff_to_perf_on_serve.m, diff_to_perf_on_serve.v + perfVarianceOnServe)
    val perf_to_skill_on_return = Gaussian(diff_to_perf_on_return.m, diff_to_perf_on_return.v + perfVarianceOnReturn)

    val p1Marginal = Gaussian(playerSkillOnServe.m, playerSkillOnServe.v) * (perf_to_skill_on_serve)
    val p2Marginal = Gaussian(playerSkillOnReturn.m, playerSkillOnReturn.v) * perf_to_skill_on_return

    (p1Marginal, p2Marginal)

  }

  /**Returns Tuple2[perf on serve,perf on return]*/
  def perfMarginals(playerSkillOnServe: Gaussian, playerSkillOnReturn: Gaussian, p1Wins: Boolean): Tuple2[MultivariateGaussian, MultivariateGaussian] = {

    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.m, playerSkillOnServe.v + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.m, playerSkillOnReturn.v + perfVarianceOnReturn)

    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val outcome_to_diff = diff_to_outcome.truncate(0, p1Wins) / diff_to_outcome

    val diff_to_perf_on_serve = outcome_to_diff + perf_on_return_to_diff
    val diff_to_perf_on_return = perf_on_serve_to_diff - outcome_to_diff

    val perf_on_serve_marginal = LinearGaussianFactor(parentVarId = 1, varId = 2, 1, 0, perfVarianceOnServe, evidence = None) *
      GaussianFactor(varId = 1, playerSkillOnServe.m, playerSkillOnServe.v) *
      GaussianFactor(varId = 2, diff_to_perf_on_serve.m, diff_to_perf_on_serve.v)

    val perf_on_return_marginal = LinearGaussianFactor(parentVarId = 1, varId = 2, 1, 0, perfVarianceOnReturn, evidence = None) *
      GaussianFactor(varId = 1, playerSkillOnReturn.m, playerSkillOnReturn.v) *
      GaussianFactor(varId = 2, diff_to_perf_on_return.m, diff_to_perf_on_return.v)

      
    (MultivariateGaussian(perf_on_serve_marginal.mean,perf_on_serve_marginal.variance), MultivariateGaussian(perf_on_return_marginal.mean,perf_on_return_marginal.variance))

  }

  def pointProb(playerSkillOnServe: Gaussian, playerSkillOnReturn: Gaussian): Double = {
    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.m, playerSkillOnServe.v + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.m, playerSkillOnReturn.v + perfVarianceOnReturn)
    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val pointProb = 1 - diff_to_outcome.cdf(0)

    pointProb
  }
}
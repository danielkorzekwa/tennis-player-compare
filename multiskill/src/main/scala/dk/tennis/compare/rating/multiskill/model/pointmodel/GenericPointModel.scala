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

  def skillMarginals(playerSkillOnServe: PlayerSkill, playerSkillOnReturn: PlayerSkill, p1Wins: Boolean): Tuple2[PlayerSkill, PlayerSkill] = {

    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.mean, playerSkillOnServe.variance + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.mean, playerSkillOnReturn.variance + perfVarianceOnReturn)

    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    var diff_to_perf_on_serve = outcome_to_diff + perf_on_return_to_diff
    val diff_to_perf_on_return = perf_on_serve_to_diff - outcome_to_diff

    var perf_to_skill_on_serve = Gaussian(diff_to_perf_on_serve.m, diff_to_perf_on_serve.v + perfVarianceOnServe)
    val perf_to_skill_on_return = Gaussian(diff_to_perf_on_return.m, diff_to_perf_on_return.v + perfVarianceOnReturn)

    val p1Marginal = Gaussian(playerSkillOnServe.mean, playerSkillOnServe.variance) * (perf_to_skill_on_serve)
    val p2Marginal = Gaussian(playerSkillOnReturn.mean, playerSkillOnReturn.variance) * perf_to_skill_on_return

    val p1MarginalFactor = PlayerSkill(p1Marginal.m, p1Marginal.v)
    val p2MarginalFactor = PlayerSkill(p2Marginal.m, p2Marginal.v)
    (p1MarginalFactor, p2MarginalFactor)

  }

  /**Returns Tuple2[perf on serve,perf on return]*/
  def perfMarginals(playerSkillOnServe: PlayerSkill, playerSkillOnReturn: PlayerSkill, p1Wins: Boolean): Tuple2[BivariateGaussianFactor, BivariateGaussianFactor] = {

    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.mean, playerSkillOnServe.variance + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.mean, playerSkillOnReturn.variance + perfVarianceOnReturn)

    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val outcome_to_diff = diff_to_outcome.truncate(0, p1Wins) / diff_to_outcome

    val diff_to_perf_on_serve = outcome_to_diff + perf_on_return_to_diff
    val diff_to_perf_on_return = perf_on_serve_to_diff - outcome_to_diff

    val perf_on_serve_marginal = LinearGaussianFactor(parentVarId = 1, varId = 2, 1, 0, perfVarianceOnServe) *
      GaussianFactor(varId = 1, playerSkillOnServe.mean, playerSkillOnServe.variance)
    GaussianFactor(varId = 2, diff_to_perf_on_serve.m, diff_to_perf_on_serve.v)

    val perf_on_return_marginal = LinearGaussianFactor(parentVarId = 1, varId = 2, 1, 0, perfVarianceOnReturn) *
      GaussianFactor(varId = 1, playerSkillOnReturn.mean, playerSkillOnReturn.variance)
    GaussianFactor(varId = 2, diff_to_perf_on_return.m, diff_to_perf_on_return.v)

    (perf_on_serve_marginal, perf_on_return_marginal)

  }

  def pointProb(playerSkillOnServe: PlayerSkill, playerSkillOnReturn: PlayerSkill): Double = {
    val perf_on_serve_to_diff = Gaussian(playerSkillOnServe.mean, playerSkillOnServe.variance + perfVarianceOnServe)
    val perf_on_return_to_diff = Gaussian(playerSkillOnReturn.mean, playerSkillOnReturn.variance + perfVarianceOnReturn)
    val diff_to_outcome = perf_on_serve_to_diff - perf_on_return_to_diff
    val pointProb = 1 - diff_to_outcome.cdf(0)

    pointProb
  }
}
package dk.tennis.compare.rating.multiskill.pointmodel

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian

case class GenericPointModel(perfVariance: Double) extends PointModel {

  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, p1Wins: Boolean): Tuple2[PlayerSkill, PlayerSkill] = {

    val p1_perf_to_diff = Gaussian(player1Skill.mean, player1Skill.variance + perfVariance)
    val p2_perf_to_diff = Gaussian(player2Skill.mean, player2Skill.variance + perfVariance)

    val diff_to_outcome = p1_perf_to_diff - p2_perf_to_diff
    val outcome_to_diff = diff_to_outcome.truncate(0, p1Wins) / diff_to_outcome

    val diff_to_perf1 = outcome_to_diff + p2_perf_to_diff
    val diff_to_perf2 = p1_perf_to_diff - outcome_to_diff

    val perf1_to_skill1 = Gaussian(diff_to_perf1.m, diff_to_perf1.v + perfVariance)
    val perf2_to_skill2 = Gaussian(diff_to_perf2.m, diff_to_perf2.v + perfVariance)

    val p1Marginal = Gaussian(player1Skill.mean, player1Skill.variance) * perf1_to_skill1
    val p2Marginal = Gaussian(player2Skill.mean, player2Skill.variance) * perf2_to_skill2

    val p1MarginalFactor = PlayerSkill(p1Marginal.m, p1Marginal.v)
    val p2MarginalFactor = PlayerSkill(p2Marginal.m, p2Marginal.v)
    (p1MarginalFactor, p2MarginalFactor)

  }

  def pointProb(player1Skill: PlayerSkill, player2Skill: PlayerSkill): Double = {
    val p1_perf_to_diff = Gaussian(player1Skill.mean, player1Skill.variance + perfVariance)
    val p2_perf_to_diff = Gaussian(player2Skill.mean, player2Skill.variance + perfVariance)
    val diff_to_outcome = p1_perf_to_diff - p2_perf_to_diff
    val pointProb = 1 - diff_to_outcome.cdf(0)

    pointProb
  }
}
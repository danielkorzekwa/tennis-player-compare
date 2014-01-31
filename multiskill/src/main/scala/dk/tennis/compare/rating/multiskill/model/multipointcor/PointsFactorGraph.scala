package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear._

case class PointsFactorGraph(p1SkillFactor: Gaussian, p2SkillFactor: Gaussian, skillCovariance: Double, p1PerfVariance: Double, p2PerfVariance: Double, pointsWon: Int, allPoints: Int) {

  private val priorSkills = CanonicalGaussian(Array(1, 2), Matrix(p1SkillFactor.m, p2SkillFactor.m), Matrix(2, 2, Array(p1SkillFactor.v, skillCovariance, skillCovariance, p2SkillFactor.v)))
  private val perf_factor = CanonicalGaussian(Array(1, 2, 3, 4), a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(p1PerfVariance, 0, 0, p2PerfVariance)))
  private val perf_diff_factor = CanonicalGaussian(Array(3, 4, 5), Matrix(1d, -1d), 0, 1e-12)

  private var p1WonPerfToSkillMsg = CanonicalGaussian(Array(1, 2), Matrix(0, 0), Matrix(2, 2, Array(Double.PositiveInfinity, 0, 0, Double.PositiveInfinity)))
  private var p2WonPerfToSkillMsg = CanonicalGaussian(Array(1, 2), Matrix(0, 0), Matrix(2, 2, Array(Double.PositiveInfinity, 0, 0, Double.PositiveInfinity)))
  private var skillsMarginal = CanonicalGaussian(Array(1, 2), Matrix(p1SkillFactor.m, p2SkillFactor.m), Matrix(2, 2, Array(p1SkillFactor.v, skillCovariance, skillCovariance, p2SkillFactor.v)))

  def getP1Marginal(): Gaussian = skillsMarginal.marginal(1).toGaussian
  def getP2Marginal(): Gaussian = skillsMarginal.marginal(2).toGaussian

  def getCovariance(): Double = skillsMarginal.getVariance.at(1)

  def sendMsgs() {
    p1WonPerfToSkillMsg = calcPerfToSkillMsgs(skillsMarginal / p1WonPerfToSkillMsg, true)
    p2WonPerfToSkillMsg = calcPerfToSkillMsgs(skillsMarginal / p2WonPerfToSkillMsg, true)
    skillsMarginal = priorSkills * power(p1WonPerfToSkillMsg, pointsWon) * power(p2WonPerfToSkillMsg, allPoints - pointsWon)
  }

  /**
   * @return perf to skill msg
   */
  private def calcPerfToSkillMsgs(skillToPerfMsg: CanonicalGaussian, p1Wins: Boolean): CanonicalGaussian = {

    val perf_to_diff = (skillToPerfMsg * perf_factor).marginalise(1).marginalise(2)

    val diff_to_outcome = (perf_to_diff * perf_diff_factor).marginal(5).toGaussian

    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    val diff_to_perf = (perf_diff_factor * CanonicalGaussian(5, outcome_to_diff.m, outcome_to_diff.v)).marginalise(5)

    val perf_to_skill = (perf_factor * diff_to_perf).marginalise(3).marginalise(4)

    perf_to_skill
  }

  private def power(gaussian: CanonicalGaussian, x: Int): CanonicalGaussian = {
    x match {
      case 0 => CanonicalGaussian(gaussian.varIds, gaussian.getMean * 0, gaussian.getVariance * Double.PositiveInfinity)
      case _ => {
        var product = gaussian
        var i = 1
        while (i < x) {
          product = product * gaussian
          i += 1
        }
        product
      }
    }
  }

}
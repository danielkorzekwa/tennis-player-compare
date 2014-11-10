package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.bayes.math.gaussian.CanonicalGaussian
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.MultivariateGaussian

case class PointsFactorGraph(directSkills: CanonicalGaussian, p1PerfVariance: Double, p2PerfVariance: Double, pointsWon: Int, allPoints: Int) extends Logging {

  require(directSkills.variance(0, 0) > 0 && directSkills.variance(1, 1) > 0, "Skill variance can't be negative")
  private val priorSkills = directSkills
  private val perf_factor = CanonicalGaussian(a = Matrix(2, 2, Array(1d, 0, 0, 1)), b = Matrix(0, 0), v = Matrix(2, 2, Array(p1PerfVariance, 0, 0, p2PerfVariance)))
  private val perf_diff_factor = CanonicalGaussian(Matrix(1d, -1d).t, 0, 1e-10)

  private var p1WonPerfToSkillMsg = CanonicalGaussian(Matrix(0, 0), Matrix(2, 2, Array(Double.PositiveInfinity, 0, 0, Double.PositiveInfinity)))
  private var p2WonPerfToSkillMsg = CanonicalGaussian(Matrix(0, 0), Matrix(2, 2, Array(Double.PositiveInfinity, 0, 0, Double.PositiveInfinity)))
  private var skillsMarginal = directSkills

  def getSkillsMarginal(): CanonicalGaussian = skillsMarginal

  def sendMsgs() {
    p1WonPerfToSkillMsg = try {
      calcPerfToSkillMsgs(skillsMarginal / p1WonPerfToSkillMsg, true)
    } catch { case e: Exception => p1WonPerfToSkillMsg }
    
    p2WonPerfToSkillMsg = try {
      calcPerfToSkillMsgs(skillsMarginal / p2WonPerfToSkillMsg, false)
    } catch { case e: Exception => p2WonPerfToSkillMsg }
    
    skillsMarginal = priorSkills * power(p1WonPerfToSkillMsg, pointsWon) * power(p2WonPerfToSkillMsg, allPoints - pointsWon)
  }

  /**
   * @return perf to skill msg
   */
  private def calcPerfToSkillMsgs(skillToPerfMsg: CanonicalGaussian, p1Wins: Boolean): CanonicalGaussian = {

    val v = Matrix(2, 2, Array(p1PerfVariance, 0, 0, p2PerfVariance))
    val perf_to_diff = MultivariateGaussian(skillToPerfMsg.mean, skillToPerfMsg.variance + v)

    val A = Matrix(1d, -1d).t
    val diff_to_outcome_m = (A * perf_to_diff.m).at(0)
    val diff_to_outcome_v = (A * perf_to_diff.v * A.t).at(0)
    val diff_to_outcome = Gaussian(diff_to_outcome_m, diff_to_outcome_v)

    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    val diff_to_perf = (perf_diff_factor * CanonicalGaussian(outcome_to_diff.m, outcome_to_diff.v).extend(3, 2)).marginalise(2)

    val perf_to_skill = (perf_factor * diff_to_perf.extend(4, 2)).marginalise(3).marginalise(2)
    //val perf_to_skill = (skillToPerfMsg.extend(4, 0)* perf_factor * diff_to_perf.extend(4, 2)).marginal(0,1) / skillToPerfMsg

    perf_to_skill
  }

  private def power(gaussian: CanonicalGaussian, x: Int): CanonicalGaussian = {
   
    x match {
      case 0 => CanonicalGaussian(gaussian.mean * 0, gaussian.variance * Double.PositiveInfinity)
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
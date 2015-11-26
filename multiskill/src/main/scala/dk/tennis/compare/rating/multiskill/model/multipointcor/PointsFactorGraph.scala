package dk.tennis.compare.rating.multiskill.model.multipointcor

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.Matrix
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian

case class PointsFactorGraph(directSkills: DenseCanonicalGaussian, p1PerfVariance: Double, p2PerfVariance: Double, pointsWon: Int, allPoints: Int) extends LazyLogging {

  require(directSkills.variance(0, 0) > 0 && directSkills.variance(1, 1) > 0, "Skill variance can't be negative")
  private val priorSkills = directSkills
  private val perf_factor = DenseCanonicalGaussian(a = new DenseMatrix(2, 2, Array(1d, 0, 0, 1)), b = DenseVector(0.0, 0), v = new DenseMatrix(2, 2, Array(p1PerfVariance, 0, 0, p2PerfVariance)))
  private val perf_diff_factor = DenseCanonicalGaussian(DenseMatrix(1d, -1d).t, 0, 1e-10)

  private var p1WonPerfToSkillMsg = DenseCanonicalGaussian(DenseVector(0.0, 0.0), new DenseMatrix(2, 2, Array(100d, 0, 0, 100d)))
  private var p2WonPerfToSkillMsg = DenseCanonicalGaussian(DenseVector(0.0, 0.0), new DenseMatrix(2, 2, Array(100d, 0, 0, 100d)))
  private var skillsMarginal = directSkills

  def getSkillsMarginal(): DenseCanonicalGaussian = skillsMarginal

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
  private def calcPerfToSkillMsgs(skillToPerfMsg: DenseCanonicalGaussian, p1Wins: Boolean): DenseCanonicalGaussian = {

    val v = new DenseMatrix(2, 2, Array(p1PerfVariance, 0, 0, p2PerfVariance)).t
    val perf_to_diff = MultivariateGaussian(skillToPerfMsg.mean, skillToPerfMsg.variance + v)

    val A = DenseMatrix(1d, -1d).t
    val diff_to_outcome_m = (A * perf_to_diff.m).apply(0)
    val diff_to_outcome_v = (A * perf_to_diff.v * A.t).apply(0,0)
    val diff_to_outcome = Gaussian(diff_to_outcome_m, diff_to_outcome_v)

    val outcome_to_diff = (diff_to_outcome.truncate(0, p1Wins)) / diff_to_outcome

    val diff_to_perf = (perf_diff_factor * DenseCanonicalGaussian(outcome_to_diff.m, outcome_to_diff.v).extend(3, 2)).marginalise(2)

    val perf_to_skill = (perf_factor * diff_to_perf.extend(4, 2)).marginalise(3).marginalise(2)
    //val perf_to_skill = (skillToPerfMsg.extend(4, 0)* perf_factor * diff_to_perf.extend(4, 2)).marginal(0,1) / skillToPerfMsg

    perf_to_skill
  }

  private def power(gaussian: DenseCanonicalGaussian, x: Int): DenseCanonicalGaussian = {

    x match {
      case 0 => DenseCanonicalGaussian(gaussian.mean * 0d, gaussian.variance * 100d)
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
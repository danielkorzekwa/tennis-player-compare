package dk.tennis.compare.rating.multiskill.infer.perfdiffgivenskills

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff
import scala.math._
import dk.bayes.math.gaussian.Gaussian
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg.Matrix

object inferPerfDiffGivenSkills {

 def apply(gameSkill: MultivariateGaussian, logPerfStdDev: Double): PerfDiff = {

    val A_d = DenseMatrix(1d, -1d).t
    val V_d = new DenseMatrix(2, 2, Array(exp(logPerfStdDev) * exp(logPerfStdDev), 0, 0, exp(logPerfStdDev) * exp(logPerfStdDev))).t

    val perfDiff = MultivariateGaussian((A_d * gameSkill.m), (A_d * (gameSkill.v + V_d) * A_d.t)).toGaussian
    PerfDiff(perfDiff, gameSkill)

  }

  def apply(p1Skill: Gaussian, p2Skill: Gaussian, logPerfStdDev: Double): PerfDiff = {

    val skillsMean = DenseVector(p1Skill.m, p2Skill.m)
    val skillsVar = new DenseMatrix(2, 2, Array(p1Skill.v, 0, 0, p2Skill.v)).t
    val gameSkills = MultivariateGaussian(skillsMean, skillsVar)

    inferPerfDiffGivenSkills(gameSkills, logPerfStdDev)

  }
}
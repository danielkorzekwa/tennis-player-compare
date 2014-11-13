package dk.tennis.compare.rating.multiskill.infer.perfdiffgivenskills

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff
import dk.bayes.math.linear.Matrix
import scala.math._
import dk.bayes.math.gaussian.Gaussian

object inferPerfDiffGivenSkills {

 def apply(gameSkill: MultivariateGaussian, logPerfStdDev: Double): PerfDiff = {

    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(exp(logPerfStdDev) * exp(logPerfStdDev), 0, 0, exp(logPerfStdDev) * exp(logPerfStdDev)))

    val perfDiff = MultivariateGaussian((A_d * gameSkill.m), (A_d * (gameSkill.v + V_d) * A_d.t)).toGaussian
    PerfDiff(perfDiff, gameSkill)

  }

  def apply(p1Skill: Gaussian, p2Skill: Gaussian, logPerfStdDev: Double): PerfDiff = {

    val skillsMean = Matrix(p1Skill.m, p2Skill.m)
    val skillsVar = Matrix(2, 2, Array(p1Skill.v, 0, 0, p2Skill.v))
    val gameSkills = MultivariateGaussian(skillsMean, skillsVar)

    inferPerfDiffGivenSkills(gameSkills, logPerfStdDev)

  }
}
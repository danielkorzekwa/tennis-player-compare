package dk.tennis.compare.rating.multiskill.model.perfdiff.math

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.linear.Matrix
import scala.math._

object gameSkillToPerfDiff {

  def apply(gameSkill: MultivariateGaussian, logPerfStdDev: Double): Gaussian = {

    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(exp(logPerfStdDev) * exp(logPerfStdDev), 0, 0, exp(logPerfStdDev) * exp(logPerfStdDev)))

    val perfDiff = MultivariateGaussian((A_d * gameSkill.m), (A_d * (gameSkill.v + V_d) * A_d.t)).toGaussian
    perfDiff

  }
}
package dk.tennis.compare.rating.multiskill.model.perfdiff

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian

case class PerfDiff(perfDiff: Gaussian, gameSkills: MultivariateGaussian) {

  def getP1Skill(): Gaussian = gameSkills.marginal(0)

  def getP2Skill(): Gaussian = gameSkills.marginal(1)
}
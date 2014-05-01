package dk.tennis.compare.rating.multiskill.model.fastgpskill.factorgraph

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.bayes.math.linear.Matrix

trait SkillsFactorGraph {

  def getSupportSkillsMarginal():MultivariateGaussian
}
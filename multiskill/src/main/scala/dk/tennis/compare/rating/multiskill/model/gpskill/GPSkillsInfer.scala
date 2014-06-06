package dk.tennis.compare.rating.multiskill.model.gpskill
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph.GPSkillsFactorGraph
import dk.bayes.math.linear.Matrix

trait GPSkillsInfer {

  /**
   * @param playerSkills Skills on serve and return for all players. Dimensions: [skills_on_serve skills_on_return]
   */
  def skillsMarginal(skillsGaussian: MultivariateGaussian, threshold: Double): GPSkillsFactorGraph

  /**
   *  Returns total log likelihood
   *  @param skillsFactorGraph Calibrated factor graph for tennis model, containing EP approximation for tennis skills posterior
   */
  def loglik(skillsFactorGraph: GPSkillsFactorGraph): Double

  /**
   * Returns partial derivative of log likelihood with respect to some parameter.
   *
   * @param skillsFactorGraph Calibrated factor graph for tennis model, containing EP approximation for tennis skills posterior
   * @param covD Element wise partial derivatives of skills covariance matrix with respect to some parameter
   */
  def loglikD(skillsFactorGraph: GPSkillsFactorGraph, covD: Matrix): Double
}
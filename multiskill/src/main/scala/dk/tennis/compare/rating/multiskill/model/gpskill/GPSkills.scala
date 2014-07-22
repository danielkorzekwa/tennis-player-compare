package dk.tennis.compare.rating.multiskill.model.gpskill
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.Gaussian

trait GPSkills {

  /**
   *  Returns total log likelihood
   */
  def loglik(): Double

  /**
   * Returns partial derivatives of log likelihood with respect to hyper parameters.
   *
   */
  def loglikD(): Array[Double]
}
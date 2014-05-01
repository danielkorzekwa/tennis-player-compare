package dk.tennis.compare.rating.multiskill.model.gpskill
import dk.bayes.math.gaussian.MultivariateGaussian

trait GPSkillsInfer {

  /**
   * @param playerSkills Skills on serve and return for all players. Dimensions: [skills_on_serve skills_on_return] 
   */
  def skillsMarginal(skillsGaussian:MultivariateGaussian,threshold: Double):MultivariateGaussian
}
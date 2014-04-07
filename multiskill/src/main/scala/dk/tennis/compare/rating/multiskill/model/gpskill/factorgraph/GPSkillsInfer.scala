package dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.bayes.math.gaussian.MultivariateGaussian

trait GPSkillsInfer {

  /**
   * @param playerSkills Skills on serve and return for all players. Dimensions: [skills_on_serve skills_on_return] 
   */
  def skillsMarginal(playerSkills:GPSkills,threshold: Double):MultivariateGaussian
}
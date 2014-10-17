package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.AllSkills

/**
 * Factor representing all skills for all players and all games.
 *
 */
trait SkillsFactor {

   def calcPosteriorSkillsByPlayerMap2(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]):AllSkills
  

}
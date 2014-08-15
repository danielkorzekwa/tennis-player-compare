package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix

/**
 * Factor representing all skills for all players and all games.
 *
 */
trait SkillsFactor {

  /**
   * Returns the prior mean of all player skills.
   *
   */
  def getPlayerSkillsPriorMean(): Matrix

  /**
   * Returns the mean of all player skills marginal.
   *
   *   @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   *
   */
  def getPlayerSkillsMarginalMean(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Matrix

  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[CanonicalGaussian]

  /**
   * Returns skills marginals and partial derivatives of skills marginals for all games with respect to log length scale of player's covariance.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   *
   * @return Tuple of (game skills marginals,derivatives of game skills marginals for n hyper parameters)
   */
  def getGameSkillsMarginalsWithD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Tuple2[Seq[CanonicalGaussian], Seq[Seq[MultivariateGaussian]]]

}
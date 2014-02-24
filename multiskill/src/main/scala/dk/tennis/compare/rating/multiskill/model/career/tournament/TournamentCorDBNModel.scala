package dk.tennis.compare.rating.multiskill.model.career.tournament

import dk.bayes.math.gaussian.Linear._
import dk.bayes.math.gaussian.CanonicalGaussian

trait TournamentCorDBNModel {

  /**
   * Returns skills on serve and return
   */
  def getAfterTSkills: CanonicalGaussian

}
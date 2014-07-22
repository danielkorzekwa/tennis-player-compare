package dk.tennis.compare.rating.multiskill.model.gpskill.multi

import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.Player
import dk.tennis.compare.rating.multiskill.model.gpskill.Score

/**
 * Multi output gp skills. GP for every player on serve/return
 *
 */
case class MultiGPSkills(hyp: Array[Double], perfVarOnServe: Double, perfVarOnReturn: Double, players: Array[Player], scores: Array[Score],
  threshold: Double = 1e-4) extends GPSkills {

  def loglik(): Double = throw new UnsupportedOperationException("Not implemented yet")

  def loglikD(): Array[Double] = throw new UnsupportedOperationException("Not implemented yet")

}
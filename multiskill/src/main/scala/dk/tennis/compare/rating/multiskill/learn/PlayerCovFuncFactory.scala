package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
trait PlayerCovFuncFactory {

  def create(params: Seq[Double], getPlayerSkill: (Player) => PlayerSkill): CovFunc
}